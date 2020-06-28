
module Dojo.Node.EventEdit
        (cgiEventEdit)
where
import Dojo.Node.EventEdit.Feed
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


-------------------------------------------------------------------------------
-- | Edit a single event, using a vertical form.
--
--    ?ee [&eid=INT] [... &u=FIELDNAME ...]
--      Show the form to edit or add an event.
--      When there is no eid argument then we add a new event.
--
--      The 'updated' fields are passed if a previous action just updated
--      the event record. Display UI feedback for these.
--
--    ?ee ... &addPerson=STRING ...
--      (action) Add people as attendees to this event,
--               then show the entry form.
--
--    ?ee ... &delPerson=PersonId ...
--      (action) Delete people as attendees to this event,
--               then show the entry form.
--
cgiEventEdit :: Session -> [(String, String)] -> CGI CGIResult
cgiEventEdit ss inputs
 = do
        -- Normalise incoming arguments.
        let Just args   = sequence $ map argOfKeyVal inputs

        -- Load form feedback from arguments.
        let fsForm      = mapMaybe takeFeedFormOfArg args
        let fsEvent     = renumberSearchFeedback
                        $ [fe | ArgFeedEvent fe <- args]

        -- Field of the event description to update.
        let fieldUpdates
                = [ (field, value) | (field@(f : _), value) <- inputs
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
        (meid, event, psAttend)
          <- case lookup "eid" inputs of
              Just strEventId
                -> do   let eid    =  read strEventId
                        event      <- liftIO $ getEvent      conn (EventId eid)
                        psAttend   <- liftIO $ getAttendance conn (EventId eid)
                        return  (Just (eventId event), event, psAttend)

              Nothing
               -> do    -- Use the current time as a placeholder until the
                        -- client provides the real event time.
                        zonedTime <- liftIO $ Time.getZonedTime
                        let (edate, etime)
                                = splitEventLocalTime
                                $ Time.zonedTimeToLocalTime zonedTime

                        let event = zeroEvent edate etime
                        return  (Nothing, event, [])

        if      -- Delete some people from the event.
                | not $ null pidsDel
                ->  cgiEventEdit_del ss conn meid pidsDel

                 -- Update the event details or add new people as attendees.
                 | (not $ null fieldUpdates) || (not $ null newNames)
                 -> cgiEventEdit_update
                        ss conn fsForm fsEvent
                        meid event psAttend fieldUpdates newNames

                 -- Show the form and wait for entry.
                 | otherwise
                 -> do  liftIO $ disconnect conn
                        outputFPS $ renderHtml
                         $ htmlEventEdit ss fsForm fsEvent meid event psAttend


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
        -> [FeedForm]
        -> [FeedEvent]
        -> Maybe EventId        -- If we're editing a pre-existing event
                                --  then just its eventId.
        -> Event                -- Event data to edit, shown in form.
        -> [Person]             -- Current attendance
        -> [(String, String)]   -- Fields to update.
        -> [String]             -- Names of people to add as attendees.
        -> CGI CGIResult

-- If the event we want to edit doesn't exist in the database yet,
-- then add now to create the event id.
cgiEventEdit_update ss conn fsForm fsEvent Nothing event psAttend updates newNames
 = do   event' <- liftIO $ insertEvent conn event
        cgiEventEdit_update ss conn fsForm fsEvent
                (Just $ eventId event') event' psAttend
                updates newNames

-- Edit an event already in the database.
cgiEventEdit_update ss conn fsForm fsEvent (Just eid) eventOld psAttend updates newNames
 = case loadEvent updates eventOld of
        -- Some of the fields didn't parse.
        Left fieldErrors
         -> do  let fsForm'
                        =  nub $ fsForm
                        ++ [ FeedFormInvalid sField sValue sError
                           | (sField, sValue, ParseError sError) <- fieldErrors ]

                outputFPS $ renderHtml
                 $ htmlEventEdit ss fsForm' fsEvent (Just eid) eventOld psAttend

        -- All the fields parsed.
        Right eventNew
         -> do  -- Add the new details to the row and see if this changes anything.
                let diffFields = diffEvent eventOld eventNew

                -- Write the event details changes to the database.
                liftIO $ updateEvent conn eventNew

                -- Feedback for updated fields.
                let fsUpdated
                     = map FeedEventFieldUpdated diffFields

                -- Find and add attendees based on the supplied names.
                fsSearch
                 <- liftM concat $ liftIO
                  $ zipWithM (searchAddPerson conn (eventId eventNew))
                              [0..] newNames
                liftIO $ commit conn
                liftIO $ disconnect conn

                -- Redirect to the same page with a clean path,
                --  that includes updated feedback.
                let fsFeed = fsUpdated ++ fsSearch
                redirect $ flatten
                 $   pathEventEdit ss (Just eid)
                 <&> mapMaybe takeKeyValOfFeedEvent fsFeed


-- | Find and add attendees based on the new names.
--   The returned arguments contain feedback as to whether we found a unique person
--   based on these search terms.
searchAddPerson
        :: IConnection conn
        => conn
        -> EventId      -- Id of the event to edit.
        -> Integer      -- Index of the form field that the search terms were in.
        -> String       -- Search terms.
        -> IO [FeedEvent]

searchAddPerson conn eid ix sQuery
 = do   found   <- findPerson conn sQuery
        case found of
         -- Found a unique person based on these terms.
         FoundOk person
          -> do  insertAttendance conn eid person
                 return [FeedEventPersonAdded (personId person)]

         -- Didn't find any people for these terms.
         FoundNone
          -> return [FeedEventSearchFoundNone ix sQuery]

         -- Found multiple people for these terms.
         FoundMany people
          -> return $ FeedEventSearchFoundMultiString ix sQuery
                    : map (FeedEventSearchFoundMultiPersonId ix)
                          (map personId people)


-------------------------------------------------------------------------------
-- | Html for event edit page.
htmlEventEdit
        :: Session
        -> [FeedForm] -> [FeedEvent]
        -> Maybe EventId -> Event -> [Person]
        -> Html

htmlEventEdit ss fsForm fsEvent mEid event psAttend
 = H.docTypeHtml
 $ do   pageHeader "Editing Event"
        pageBody
         $ do   (if isJust mEid
                  then do tablePaths $ pathsJump ss
                          tablePaths [pathEventView ss $ eventId event]
                  else do tablePaths (pathsJump ss))

                -- Main entry form.
                H.div   ! A.class_ "event"
                        $ formEvent (pathEventEdit ss mEid) fsForm fsEvent
                                event psAttend
