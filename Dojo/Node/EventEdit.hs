
module Dojo.Node.EventEdit
        (cgiEventEdit)
where
import Dojo.Node.EventEdit.Form
import Dojo.Node.EventEdit.Arg
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Data.Session
import Dojo.Paths
import Dojo.Chrome
import Dojo.Framework
import Dojo.Base
import Config
import qualified Data.Time                      as Time
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | Edit a single event, using a vertical form.
--
--    ?eEdit [&eid=INT] [... &p=FIELDNAME ...]
--      Show the form to edit or add an event.
--      When there is no eid argument then we add a new event.
--
--      The 'updated' fields are passed if a previous action just updated
--      the event record. Display UI feedback for these.
--
--    ?eEdit ... &addPerson=STRING ...
--      (action) Add people as attendees to this event,
--               then show the entry form.
--
--    ?eEdit ... &delPerson=PersonId ...
--      (action) Delete people as attendees to this event,
--               then show the entry form.
--
cgiEventEdit :: Session -> [(String, String)] -> CGI CGIResult
cgiEventEdit ss inputs
 = do
        -- Normalise incoming arguments.
        let Just args
                = liftM renumberSearchFeedback
                $ sequence
                $ map argOfKeyVal inputs

        -- Field of the event description to update.
        let fieldUpdates
                = [ (field, value)      | (field@(f : _), value) <- inputs
                                        , isUpper f ]

        -- People to delete from the event.
        let pidsDel  = [ pid  | ArgDelPerson pid  <- args ]

        -- People to add to this event.
        let newNames = [ name | ArgAddPerson name <- args ]

        -- Connect to the database.
        conn <- liftIO $ connectSqlite3 databasePath

        -- If we have an existing eventId then load the existing event data,
        --  otherwise start with an empty event record,
        --  using the current time as a placeholder.
        (meid, event, attendance)
          <- case lookup "eid" inputs of
              Just strEventId
                -> do   let eid    =  read strEventId
                        event      <- liftIO $ getEvent      conn (EventId eid)
                        attendance <- liftIO $ getAttendance conn (EventId eid)
                        return  (Just (eventId event), event, attendance)

              Nothing
               -> do    -- Use the current time as a placeholder until we have
                        -- the real time.
                        zonedTime       <- liftIO $ Time.getZonedTime
                        let (edate, etime)
                                        = splitEventLocalTime
                                        $ Time.zonedTimeToLocalTime zonedTime

                        let event       = zeroEvent edate etime
                        return  (Nothing, event, [])

        let result
                 -- Delete some people from the event.
                 | not $ null pidsDel
                 =      cgiEventEdit_del
                                ss conn meid pidsDel

                 -- Update the event details or add new people as attendees.
                 | (not $ null fieldUpdates) || (not $ null newNames)
                 =      cgiEventEdit_update
                                ss conn meid event fieldUpdates newNames

                 -- Show the form and wait for entry.
                 | otherwise
                 = do   liftIO $ disconnect conn
                        cgiEventEdit_entry
                                ss args meid event attendance

        result


-------------------------------------------------------------------------------
-- Delete some people from the event.
cgiEventEdit_del
        :: IConnection conn
        => Session
        -> conn
        -> Maybe EventId
        -> [PersonId]
        -> CGI CGIResult

cgiEventEdit_del ss conn meid pids
 = case meid of
        -- If we have no eid then the initial event record hasn't been added
        -- to the database yet. It doesn't have any attendance on it, so we
        -- can just return without doing anything.
        Nothing
         -> do  liftIO   $ disconnect conn
                redirect $ flatten $ pathEventEdit ss Nothing

        -- We have an event record in the database, so need to actually delete
        -- attendance from it.
        Just eid
         -> do  liftIO   $ mapM_ (deleteAttendance conn eid) pids
                liftIO   $ commit conn
                liftIO   $ disconnect conn
                redirect $ flatten $ pathEventEdit ss (Just eid)


-------------------------------------------------------------------------------
-- We got some updates.
--  Update the database and show the updated form.
cgiEventEdit_update
        :: IConnection conn
        => Session
        -> conn
        -> Maybe EventId        -- If we're editing a pre-existing event
                                --  then just its eventId.
        -> Event                -- Event data to edit, shown in form.
        -> [(String, String)]   -- Fields to update.
        -> [String]             -- Names of people to add as attendees.
        -> CGI CGIResult

-- If the event we want to edit doesn't exist in the database yet,
-- then add it and look it up again to get its event Id.
-- NOTE: We rely on the local time in this event being a globally unique event key.
cgiEventEdit_update ss conn Nothing event updates newNames
 = do
        liftIO $ insertEvent conn event
        liftIO $ commit conn

        let EventLocalTime ltime        = eventLocalTime event
        event'  <- liftIO $ getEventOfLocalTime conn ltime
        cgiEventEdit_update ss conn (Just $ eventId event') event' updates newNames

-- Edit an event already in the database.
cgiEventEdit_update ss conn (Just eid) event updates newNames

 -- Try to parse the new details
 = case loadEvent updates event of
    -- Some of the fields didn't parse.
    Left fieldErrs
     -> redirect $ flatten
      $ pathEventEdit ss (Just eid)
                <&> map keyValOfArg
                        [ ArgDetailsInvalid name str
                        | (name, str, _) <- fieldErrs ]

    -- All the fields parsed.
    Right event'
     -> do
        -- Add the new details to the row and see if this changes anything.
        let diffFields = diffEvent event   event'

        -- Write the event details changes to the database.
        liftIO $ updateEvent conn event'

        -- Find and add attendees based on the supplied names.
        fsSearchFeedback
                <- liftM concat
                $  liftIO
                $  zipWithM (searchAddPerson conn (eventId event))
                        [0..] newNames

        liftIO $ commit conn
        liftIO $ disconnect conn

        -- Stay on the same page, but show what fields were updated.
        redirect $ flatten
         $ pathEventEdit ss (Just eid)
                <&> map keyValOfArg (map ArgDetailsUpdated diffFields)
                <&> map keyValOfArg fsSearchFeedback


-- | Find and add attendees based on the new names.
--   The returned arguments contain feedback as to whether we found a unique person
--   based on these search terms.
searchAddPerson
        :: IConnection conn
        => conn
        -> EventId      -- Id of the event to edit.
        -> Integer      -- Index of the form field that the search terms were in.
        -> String       -- Search terms.
        -> IO [Arg]

searchAddPerson conn eid ix names
 = do   found   <- findPerson conn names

        case found of
         -- Found a unique person based on these terms.
         FoundOk person
          -> do  insertAttendance conn eid person
                 return [ArgPersonAdded (personId person)]

         -- Didn't find any people for these terms.
         FoundNone
          -> return [ArgSearchFoundNone ix names]

         -- Found multiple people for these terms.
         FoundMany people
          -> return $ ArgSearchFoundMultiString ix names
                    : map (ArgSearchFoundMultiPersonId ix) (map personId people)


-------------------------------------------------------------------------------
-- | We haven't got any updates yet, so show the entry form.
cgiEventEdit_entry
        :: Session
        -> [Arg]
        -> Maybe EventId        -- If we're editing a pre-existing event
                                --   then just its eventId.
        -> Event                -- Event data to edit, shown in form.
        -> [Person]             -- Current attendance to this event.
        -> CGI CGIResult

cgiEventEdit_entry ss args meid event attendance
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Editing Event"
        pageBody
         $ do   (if isJust meid
                  then tablePaths (pathsJump ss ++ [pathEventView ss $ eventId event])
                  else tablePaths (pathsJump ss))

                -- Main entry form.
                H.div   ! A.class_ "event"
                        $ formEvent args
                                (pathEventEdit ss meid)
                                event attendance

