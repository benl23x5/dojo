
module Dojo.Node.EventEditAttend (cgiEventEditAttend) where
import Dojo.Node.EventEdit.Base
import Dojo.Node.EventEdit.Search
import Dojo.Node.EventEdit.Feed
import Dojo.Node.EventEdit.FormAttend
import Dojo.Node.EventEdit.Arg
import Dojo.Node.Logout
import Dojo.Data.Event
import Dojo.Data.Class
import Dojo.Data.Session
import Dojo.Data.Dojo
import Dojo.Paths
import Dojo.Fail
import Dojo.Chrome
import Dojo.Framework

import qualified Data.Time                      as Time
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | Edit a single event, using a vertical form.
--
--    ?ee [&eid=INT] ...
--
--    ?ee ... &addPerson=PersonId ...
--      (action) Add people as attendees to this event,
--               then show the entry form.
--
--    ?ee ... &delPerson=PersonId ...
--      (action) Delete people as attendees to this event,
--               then show the entry form.
--
--    ?ee ... &addName=STRING ...
--      (action) Add people as attendees to this event,
--               then show the entry form.
--
--    ?ee ... &fa=PID ...
--      (feedback) Feedback person added.
--
--
cgiEventEditAttend :: Session -> [(String, String)] -> CGI CGIResult
cgiEventEditAttend ss inputs
 = do   -- Connect to the database.
        conn <- liftIO $ connectSqlite3 $ sessionDatabasePath ss

        -- Normalise incoming arguments.
        let ssArgs = filter (\(k, _) -> not $ elem k ["s", "n", "eid"]) inputs
        let args   = case sequence $ map argOfKeyVal ssArgs of
                        Just aa -> aa
                        Nothing -> throw $ FailNodeArgs "event edit" inputs

        -- Parse an existing event id if we were given one.
        mEidIn
         <- case lookup "eid" inputs of
                Just strEid
                 -> case parse strEid of
                     Left err  -> throw $ FailParse "event id" strEid err
                     Right eid -> return $ Just eid
                Nothing -> return Nothing

        -- Split out fields.
        let updates  = [ (field, value) | ArgUpdate field value <- args ]
        let pidsAdd  = [ pid  | ArgAddPerson pid <- args ]
        let pidsDel  = [ pid  | ArgDelPerson pid <- args ]
        let newNames = [ name | ArgAddName name <- args ]

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

        -- Get current dims lists.
        dojos      <- liftIO $ getDojos conn
        eventTypes <- liftIO $ getEventTypes conn


        -- Lookup the current event and attendance if one was specified,
        --  otherwise create a new placeholder that we can update.
        (event, psAttend, psRegulars) <- if
         | Just eid <- mEidIn
         -> do  -- TODO: handle concurrent event deletion
                Just event <- liftIO $ getEvent      conn eid
                psAttend   <- liftIO $ getAttendance conn eid

                -- Lookup a matching class id if there is one,
                -- which will give us regular attendees to events
                -- of this class.
                mCid       <- liftIO $ getClassIdOfEventId conn eid
                psRegularsCountLast
                 <- case mCid of
                        Nothing  -> return []
                        Just cid -> liftIO $ getRecentRegularsOfClassId conn cid

                let psRegulars
                        = [ pReg | (pReg, _nCount, _dateLast)
                                 <- psRegularsCountLast]

                return (event, psAttend, psRegulars)

         | otherwise
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
                return  (event, [], [])

        (userCreatedBy, personCreatedBy)
         <- liftIO $ getEventCreatedBy conn event

        -- If user is an admin or created the event themselves
        --  then they can edit it.
        let bSessionOwnsEvent
             =  sessionIsAdmin ss
             || (case eventCreatedBy event of
                  Nothing  -> False
                  Just uid -> uid == sessionUserId ss)

        let eform
                = EventForm
                { eventFormPath                = pathEventEditAttend ss mEidIn
                , eventFormFeedForm            = fsForm
                , eventFormFeedEvent           = fsEvent
                , eventFormEventValue          = event
                , eventFormEventTypes          = eventTypes
                , eventFormAttendance          = psAttend
                , eventFormRegulars            = psRegulars
                , eventFormDojosAvail          = dojos
                , eventFormDetailsEditable     = True
                , eventFormAttendanceDeletable = True
                , eventFormCreatedByUser       = Just $ userCreatedBy
                , eventFormCreatedByPerson     = Just $ personCreatedBy }

        if
         -- Delete attendees from the event by person id.
         | not $ null pidsDel
         -> if bSessionOwnsEvent
                then cgiEventEdit_del ss conn mEidIn pidsDel
                else cgiLogout ss

         -- Apply updates or add new people based on their names.
         |   (not $ null updates)
          || (not $ null newNames)
          || (not $ null pidsAdd)
         -> if bSessionOwnsEvent
                then cgiEventEdit_update
                        ss conn mEidIn eform updates pidsAdd newNames
                else cgiLogout ss

         -- Show form to update an existing event,
         --  which is possible for admins and the user that created them.
         | isJust mEidIn
         -> if bSessionOwnsEvent
                then do liftIO $ disconnect conn
                        cgiEventEditAttendForm ss eform
                else cgiLogout ss

         -- Show the form to create a new event,
         --  which is possible for everyone.
         --  TODO: suppress editability if not owner.
         | otherwise
         -> do  liftIO $ disconnect conn
                cgiEventEditAttendForm ss eform


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
                redirect $ flatten $ pathEventEditAttend ss Nothing

        -- We have an event record in the database, so need to actually delete
        -- attendance from it.
        Just eid
         -> do  liftIO   $ mapM_ (deleteAttendance conn eid) pids
                liftIO   $ commit conn
                liftIO   $ disconnect conn
                redirect $ flatten $ pathEventEditAttend ss (Just eid)


-------------------------------------------------------------------------------
-- We got some updates.
--  Update the database and show the updated form.
cgiEventEdit_update
        :: IConnection conn
        => Session
        -> conn
        -> Maybe EventId        -- If we're editing a pre-existing event
                                --  then just its eventId.
        -> EventForm             -- Event data to edit, shown in form.
        -> [(String, String)]   -- Fields to update.
        -> [PersonId]           -- pids of people to add as attendees
        -> [String]             -- Names of people to add as attendees.
        -> CGI CGIResult

-- If the event we want to edit doesn't exist in the database yet,
-- then add now to create the event id.
cgiEventEdit_update
        ss conn Nothing
        eform updates pidsAdd newNames
 = do   event' <- liftIO $ insertEvent conn $ eventFormEventValue eform
        cgiEventEdit_update
                ss conn (eventId event')
                eform { eventFormEventValue = event'}
                updates pidsAdd newNames

-- Edit an event already in the database.
cgiEventEdit_update
        ss conn (Just eid)
        eform updates pidsAdd newNames
 = case loadEvent updates (eventFormEventValue eform) of
        Left fieldErrors -> goError fieldErrors
        Right eventNew   -> goNewEvent eventNew
 where
  goError fieldErrors
   = do
        let fsForm'
                =  nub $ (eventFormFeedForm eform)
                ++ [ FeedFormInvalid sField sValue sError
                   | (sField, sValue, ParseError sError) <- fieldErrors ]

        let eform'
                = eform
                { eventFormFeedForm            = fsForm' }

        cgiEventEditAttendForm ss eform'

  goNewEvent eventNew
   = do -- Get the fields that have been updated by the form.
        let diffFields = diffEvent (eventFormEventValue eform) eventNew

        -- Write the event details changes to the database.
        liftIO $ updateEvent conn eventNew

        -- Feedback for updated fields.
        let fsUpdated   = map FeedEventFieldUpdated diffFields

        let psAttend    = eventFormAttendance eform

        -- Add attendees based on provided person ids.
        fsAddById
         <- liftM concat $ liftIO
         $  mapM (addPersonByPersonId conn eventNew psAttend) pidsAdd

        -- Find and add attendees based on the supplied names.
        fsAddByName
         <- liftM concat $ liftIO
         $  zipWithM (addPersonByName conn eventNew psAttend) [0..] newNames

        liftIO $ commit conn
        liftIO $ disconnect conn

        -- Redirect to the same page with a clean path,
        --  that includes updated feedback.
        let fsFeed = fsUpdated ++ fsAddById ++ fsAddByName
        redirect $ flatten
         $   pathEventEditAttend ss (Just eid)
         <&> mapMaybe takeKeyValOfFeedEvent fsFeed


-------------------------------------------------------------------------------
-- | Html for event edit page.
cgiEventEditAttendForm :: Session -> EventForm -> CGI CGIResult
cgiEventEditAttendForm ss eform
 = cgiPageNavi "Events" "Editing Attendance" (pathsJump ss)
 $ H.div ! A.class_ "event" $ formEventAttend ss eform

