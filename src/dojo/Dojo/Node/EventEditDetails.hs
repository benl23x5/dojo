
module Dojo.Node.EventEditDetails (cgiEventEditDetails) where
import Dojo.Node.EventEdit.Base
import Dojo.Node.EventEdit.Feed
import Dojo.Node.EventEdit.FormDetails
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
--    ?ee ... &fu=FieldName ...
--      (feedback) Feedback field updated.
--
--    ?ee ... &fa=PID ...
--      (feedback) Feedback person added.
--
--
cgiEventEditDetails :: Session -> [(String, String)] -> CGI CGIResult
cgiEventEditDetails ss inputs
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

        -- Load form feedback from arguments.
        let fsForm = mapMaybe takeFeedFormOfArg args

        -- Get current dims lists.
        dojos      <- liftIO $ getDojos conn
        eventTypes <- liftIO $ getEventTypes conn

        -- Lookup the current event and attendance if one was specified,
        --  otherwise create a new placeholder that we can update.
        event <- if
         | Just eid <- mEidIn
         -> do  Just event <- liftIO $ getEvent      conn eid
                return event

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
                return event

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
                { eventFormPath                = pathEventEditDetails ss mEidIn
                , eventFormFeedForm            = fsForm
                , eventFormFeedEvent           = []
                , eventFormEventValue          = event
                , eventFormEventTypes          = eventTypes
                , eventFormAttendance          = []
                , eventFormRegulars            = []
                , eventFormDojosAvail          = dojos
                , eventFormDetailsEditable     = True
                , eventFormAttendanceDeletable = True
                , eventFormCreatedByUser       = Just $ userCreatedBy
                , eventFormCreatedByPerson     = Just $ personCreatedBy }

        if
         -- Apply updates
         |   (not $ null updates)
         -> if bSessionOwnsEvent
                then cgiEventEdit_update ss conn mEidIn eform updates
                else cgiLogout ss

         -- Show form to update an existing event,
         --  which is possible for admins and the user that created them.
         | isJust mEidIn
         -> if bSessionOwnsEvent
                then do liftIO $ disconnect conn
                        cgiEventEditDetailsForm ss eform
                else cgiLogout ss

         -- Show the form to create a new event,
         --  which is possible for everyone.
         --  TODO: suppress editability if not owner.
         | otherwise
         -> do  liftIO $ disconnect conn
                cgiEventEditDetailsForm ss eform


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
        -> CGI CGIResult

-- If the event we want to edit doesn't exist in the database yet,
-- then add now to create the event id.
cgiEventEdit_update
        ss conn Nothing
        eform updates
 = do   event' <- liftIO $ insertEvent conn $ eventFormEventValue eform
        cgiEventEdit_update
                ss conn (eventId event')
                eform { eventFormEventValue = event'}
                updates

-- Edit an event already in the database.
cgiEventEdit_update
        ss conn (Just eid)
        eform updates
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

        cgiEventEditDetailsForm ss eform'

  goNewEvent eventNew
   = do -- Get the fields that have been updated by the form.
        let diffFields = diffEvent (eventFormEventValue eform) eventNew

        -- Write the event details changes to the database.
        liftIO $ updateEvent conn eventNew

        -- Feedback for updated fields.
        let fsUpdated   = map FeedEventFieldUpdated diffFields

        liftIO $ commit conn
        liftIO $ disconnect conn

        redirect $ flatten
         $   pathEventEditDetails ss (Just eid)
         <&> mapMaybe takeKeyValOfFeedEvent fsUpdated


-------------------------------------------------------------------------------
-- | Html for event edit page.
cgiEventEditDetailsForm :: Session -> EventForm -> CGI CGIResult
cgiEventEditDetailsForm ss eform
 = cgiPageNavi "Editing Event" (pathsJump ss)
 $ H.div ! A.class_ "event" $ formEventDetails ss eform
