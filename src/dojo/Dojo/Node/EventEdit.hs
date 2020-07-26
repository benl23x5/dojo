
module Dojo.Node.EventEdit (cgiEventEdit) where
import Dojo.Node.EventEdit.Feed
import Dojo.Node.EventEdit.Form
import Dojo.Node.EventEdit.Arg
import Dojo.Node.Logout
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Data.Session
import Dojo.Data.Dojo
import Dojo.Paths
import Dojo.Fail
import Dojo.Chrome
import Config
import qualified Data.Time                      as Time
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | Edit a single event, using a vertical form.
--
--    ?ee [&eid=INT] ...
--
--    ?ee ... &addPerson=STRING ...
--      (action) Add people as attendees to this event,
--               then show the entry form.
--
--    ?ee ... &delPerson=PersonId ...
--      (action) Delete people as attendees to this event,
--               then show the entry form.
--
--    ?ee ... &fu=FieldName ...
--      (feedback) Feedback field updated.
--
--    ?ee ... &fa=PID ...
--      (feedback) Feedback person added.
--
--
cgiEventEdit :: Session -> [(String, String)] -> CGI CGIResult
cgiEventEdit ss inputs
 = do   -- Connect to the database.
        conn <- liftIO $ connectSqlite3 databasePath

        -- Normalise incoming arguments.
        let ssArgs = filter (\(k, _) -> not $ elem k ["s", "n", "eid"]) inputs
        let args   = case sequence $ map argOfKeyVal ssArgs of
                        Just aa -> aa
                        Nothing -> throw $ FailNodeArgs "event edit" inputs

        -- Load form feedback from arguments.
        let fsForm = mapMaybe takeFeedFormOfArg args

        -- If there is feedback saying that a person search found multiple
        --  matching person ids then lookup the full details so we can display
        --  the multiple names in feedback. Also renumber the feedback so any
        --  search terms that are still unresolved are packed to the front
        --  of the input list.
        fsEvent <- liftIO $ fmap concat
                $  mapM (expandMultiPersonId conn)
                $  renumberSearchFeedback
                $  [fe | ArgFeedEvent fe <- args]

        -- Extract fields to update from the arguments.
        let updates  = [ (field, value) | ArgUpdate field value <- args ]

        -- People to delete from the event.
        let pidsDel  = [ pid  | ArgDelPerson pid  <- args ]

        -- People to add to this event.
        let newNames = [ name | ArgAddPerson name <- args ]

        -- Get current dims lists.
        dojos      <- liftIO $ getDojos conn
        eventTypes <- liftIO $ getEventTypes conn

        -- Parse an existing event id if we were given one.
        mEidIn
         <- case lookup "eid" inputs of
                Just strEid
                 -> case parse strEid of
                     Left err  -> throw $ FailParse "event id" strEid err
                     Right eid -> return $ Just eid
                Nothing -> return Nothing

        -- Lookup the current event and attendance if one was specified,
        --  otherwise create a new placeholder that we can update.
        (event, psAttend)
         <- case mEidIn of
                Just eid
                 -> do  event    <- liftIO $ getEvent      conn eid
                        psAttend <- liftIO $ getAttendance conn eid
                        return (event, psAttend)

                Nothing
                 -> do  -- Use the current time as a placeholder until the
                        -- client provides the real event time.
                        zonedTime <- liftIO $ Time.getZonedTime
                        let (edate, etime)
                                = splitEventLocalTime
                                $ Time.zonedTimeToLocalTime zonedTime

                        let event = zeroEvent
                                  { eventDate      = Just edate
                                  , eventTime      = Just etime
                                  , eventCreatedBy = Just $ sessionUserId ss }
                        return  (event, [])

        -- If user is an admin or created the event themselves
        --  then they can edit it.
        let bSessionOwnsEvent
             =  sessionIsAdmin ss
             || (case eventCreatedBy event of
                  Nothing  -> False
                  Just uid -> uid == sessionUserId ss)

        if
         -- Apply deletions of attendees from the event.
         | not $ null pidsDel
         -> if bSessionOwnsEvent
                then cgiEventEdit_del ss conn mEidIn pidsDel
                else cgiLogout ss

         -- Apply updates or add new people as attendees.
         | (not $ null updates) || (not $ null newNames)
         -> if bSessionOwnsEvent
                then cgiEventEdit_update
                        ss conn fsForm fsEvent
                        mEidIn event psAttend dojos eventTypes
                        updates newNames
                else cgiLogout ss

         -- Show form to update an existing event,
         --  which is possible for admins and the user that created them.
         | isJust mEidIn
         -> if bSessionOwnsEvent
                then do liftIO $ disconnect conn
                        outputFPS $ renderHtml
                         $ htmlEventEdit ss fsForm fsEvent
                                mEidIn event psAttend dojos eventTypes
                else cgiLogout ss

         -- Show the form to create a new event,
         --  which is possible for everyone.
         | otherwise
         -> do  liftIO $ disconnect conn
                outputFPS $ renderHtml
                 $ htmlEventEdit ss fsForm fsEvent
                        mEidIn event psAttend dojos eventTypes


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
        -> [PersonDojo]         -- Current dojos list
        -> [EventType]          -- Current event types.
        -> [(String, String)]   -- Fields to update.
        -> [String]             -- Names of people to add as attendees.
        -> CGI CGIResult

-- If the event we want to edit doesn't exist in the database yet,
-- then add now to create the event id.
cgiEventEdit_update
        ss conn fsForm fsEvent
        Nothing event psAttend dojos eventTypes updates newNames
 = do   event' <- liftIO $ insertEvent conn event
        cgiEventEdit_update
                ss conn fsForm fsEvent
                (eventId event') event'
                psAttend dojos eventTypes
                updates newNames

-- Edit an event already in the database.
cgiEventEdit_update
        ss conn fsForm fsEvent
        (Just eid) eventOld psAttend dojos eventTypes updates newNames
 = case loadEvent updates eventOld of
        Left fieldErrors -> goError fieldErrors
        Right eventNew   -> goNewEvent eventNew
 where
  goError fieldErrors
   = do let fsForm'
                =  nub $ fsForm
                ++ [ FeedFormInvalid sField sValue sError
                   | (sField, sValue, ParseError sError) <- fieldErrors ]

        outputFPS $ renderHtml
         $ htmlEventEdit ss
                fsForm' fsEvent
                (Just eid) eventOld
                psAttend dojos eventTypes

  goNewEvent eventNew
   = do -- Get the fields that have been updated by the form.
        let diffFields = diffEvent eventOld eventNew

        -- Write the event details changes to the database.
        liftIO $ updateEvent conn eventNew

        -- Feedback for updated fields.
        let fsUpdated
             = map FeedEventFieldUpdated diffFields

        -- Find and add attendees based on the supplied names.
        fsSearch
         <- liftM concat $ liftIO
          $ zipWithM (searchAddPerson conn eventNew psAttend)
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
--   The returned arguments contain feedback as to whether we found a
--   unique person based on these search terms.
searchAddPerson
        :: IConnection conn
        => conn
        -> Event    -- ^ Event that we're currently editing.
        -> [Person] -- ^ People we know are already attending this event.
        -> Integer  -- ^ Index of the field containing the search string.
        -> String   -- ^ Search string.
        -> IO [FeedEvent]

searchAddPerson conn event psAttend ix sQuery
 = do
        -- Find more people based on the search string,
        -- skipping over people that are already in the list.
        let pidsSkip = mapMaybe personId psAttend
        found <- findPerson conn sQuery pidsSkip

        case found of
         -- Found a unique person based on these terms.
         FoundOk person
          -- .. but the person is already an attendee, so skip.
          | elem (personId person) $ map personId psAttend
          -> return []

          -- .. and the person is not already an attendee, so add them.
          | Just eid <- eventId event
          -> do  insertAttendance conn eid person
                 return [ FeedEventPersonAdded pid
                        | Just pid <- [personId person]]

          -- .. no event id yet, skip.
          | otherwise
          -> return []

         -- Didn't find any people for these terms.
         FoundNone
          -> return [FeedEventSearchFoundNone ix sQuery]

         -- Found multiple people for these terms.
         FoundMany people
          -> return $ FeedEventSearchFoundMultiString ix sQuery
                    : map (FeedEventSearchFoundMultiPersonId ix)
                          (take 6 $ mapMaybe personId people)


-------------------------------------------------------------------------------
-- | Html for event edit page.
htmlEventEdit
        :: Session
        -> [FeedForm] -> [FeedEvent]
        -> Maybe EventId -> Event -> [Person] -> [PersonDojo] -> [EventType]
        -> Html

htmlEventEdit ss fsForm fsEvent mEid event psAttend dojos eventTypes
 = H.docTypeHtml
 $ do   pageHeader "Editing Event"
        pageBody
         $ do   tablePaths $ pathsJump ss

                tablePaths
                 $ case eventId event of
                    Nothing     -> []
                    Just eid    -> [pathEventView ss eid]

                H.div ! A.class_ "event"
                 $ formEvent (pathEventEdit ss mEid)
                        fsForm fsEvent
                        event eventTypes psAttend dojos
