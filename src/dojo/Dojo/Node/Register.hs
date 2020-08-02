
module Dojo.Node.Register (cgiRegister) where
import Dojo.Node.ClassView
import Dojo.Node.EventEdit.Search
import Dojo.Node.EventEdit.Form
import Dojo.Node.EventEdit.Feed
import Dojo.Node.EventEdit.Arg
import Dojo.Data.User
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Data.Class
import Dojo.Chrome
import Dojo.Config
import Dojo.Paths
import Dojo.Fail
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Data.Time                      as Time
import qualified Data.Time.Calendar.OrdinalDate as Time


-------------------------------------------------------------------------------
-- | Registration page for specific class.
--     &r=REGID
cgiRegister
 :: Config
 -> [(String, String)]  -- ^ Inputs
 -> String              -- ^ Registration Id.
 -> CGI CGIResult

cgiRegister cc inputs sRegId
 = do   conn    <- liftIO $ connectSqlite3 $ configDatabasePath cc
        classs  <- liftIO $ getClasses conn

        -- Normalise incoming arguments.
        let ssArgs = filter (\(k, _) -> not $ elem k ["r"]) inputs
        let args   = case sequence $ map argOfKeyVal ssArgs of
                        Just aa -> aa
                        Nothing -> throw $ FailNodeArgs "event edit" inputs

        -- If there is feedback saying that a person search found multiple
        --  matching person ids then lookup the full details so we can display
        --  the multiple names in feedback. Also renumber the feedback so any
        --  search terms that are still unresolved are packed to the front
        --  of the input list.
        fsEvent <- liftIO $ fmap concat
                $  mapM (expandMultiPersonId conn)
                $  renumberSearchFeedback
                $  [fe | ArgFeedEvent fe <- args]

        -- People to add to this event.
        let newNames = [ name | ArgAddPerson name <- args ]

        let sUrl  = configSiteUrl cc
        let sSalt = configQrSaltActive cc

        let lsActive
              =  catMaybes $ flip map classs $ \cls
              -> case registrationLinkOfClass sUrl sSalt cls of
                  Just (_sUrl, sId) -> Just (sId, cls)
                  Nothing           -> Nothing

        case lookup sRegId lsActive of
         Just cls -> cgiRegister_class cc conn sRegId cls fsEvent newNames
         Nothing  -> cgiRegister_unknown sRegId


cgiRegister_unknown sRegId
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Register"
        H.string $ "registration link unknown " ++ sRegId


-------------------------------------------------------------------------------
cgiRegister_class cc conn sRegId cls fsEvent newNames
 = do
        -- Get all the events for the specified class.
        let Just cid = classId cls
        eventList <- liftIO $ getEventsOfClassId conn cid

        -- See if there is an event created for today already.
        zonedTime <- liftIO $ Time.getZonedTime
        let (edate, _etime)
                = splitEventLocalTime
                $ Time.zonedTimeToLocalTime zonedTime

        let eventsToday
                = [ event | (event, _pax) <- eventList
                  , eventDate event == Just edate ]

        case eventsToday of
         [] -> cgiRegister_create conn cls

         [event]
          -> do let Just eid = eventId event
                cgiRegister_existing
                        cc conn sRegId cls event eid edate
                        fsEvent newNames

         events
            -> cgiRegister_multi conn sRegId cls events


-------------------------------------------------------------------------------
-- There are already multiple events belonging to the same class,
--   which should only happen if an admin has messed something up.
--   We don't currently have a constraint in the db to prevent this.
cgiRegister_multi _conn sRegId cls events
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Register"
        H.string $ "multiple events exist for the class"
                ++ sRegId ++ " " ++ show cls ++ " " ++ show events


-------------------------------------------------------------------------------
-- We are trying to register for a class but there is not event
-- record for it yet.
cgiRegister_create _conn cls
 = do
        -- First decide if the class is actually on today.
        zonedTime    <- liftIO $ Time.getZonedTime
        let localTime = Time.zonedTimeToLocalTime zonedTime
        let (_, nDay) = Time.sundayStartWeek $ Time.localDay localTime
        let sDay      = ssDays !! nDay

        if classDay cls == Just (ClassDay sDay)
         then cgiRegister_create_new cls
         else cgiRegister_create_notToday cls sDay

 where  ssDays :: [String]
         = [ "Sunday", "Monday", "Tuesday"
           , "Wednesday", "Thursday", "Friday", "Saturday" ]

cgiRegister_create_new cls
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Event Registration"
        pageBody
         $ H.div ! A.class_ "event"
         $ do   H.h2 "Event Registration"

                H.table
                 $ trClassSummary cls

                H.string $ "need to make a new event" ++ show cls


cgiRegister_create_notToday cls sDay
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Event Registration"
        pageBody
         $ H.div ! A.class_ "event"
         $ do   H.h2 "Event Registration"

                H.table
                 $ trClassSummary cls

                H.br
                H.string $ "Today is " ++ sDay
                H.br
                H.string "This class is not on today."


-------------------------------------------------------------------------------
-- There is a single existing event record today for this class.
cgiRegister_existing
        cc conn sRegId cls event eid edate
        fsEvent newNames
 | not $ null newNames
 = cgiRegister_existing_update
        cc conn sRegId cls event eid edate
        newNames

 | otherwise
 = cgiRegister_existing_form
        cc conn sRegId cls event eid edate
        fsEvent


cgiRegister_existing_update
        cc conn sRegId _cls event eid _edate
        newNames
 = do
        psAttend <- liftIO $ getAttendance conn eid

        fsSearch <- liftM concat $ liftIO
                 $  zipWithM (searchAddPerson conn event psAttend)
                        [0..] newNames

        liftIO $ commit conn
        liftIO $ disconnect conn

        -- Redirect to the same page with a clean path,
        --  that includes updated feedback.
        redirect $ flatten
         $   pathRegister cc sRegId
         <&> mapMaybe takeKeyValOfFeedEvent fsSearch


cgiRegister_existing_form
        cc conn sRegId _cls event eid _edate
        fsEvent
 = do
        psAttend <- liftIO $ getAttendance conn eid

        (userCreatedBy, personCreatedBy)
         <- do  let Just uidCreatedBy = eventCreatedBy event
                Just user <- liftIO $ getUserOfId conn uidCreatedBy
                person    <- liftIO $ getPerson conn $ userPersonId user
                return (user, person)

        liftIO $ disconnect conn

        outputFPS $ renderHtml
         $ H.docTypeHtml
         $ do   pageHeader "Event Registration"
                pageBody
                 $ H.div ! A.class_ "event"
                 $ do   H.h2 "Event Registration"
                        formEvent $ EventForm
                         { eventFormPath                = pathRegister cc sRegId
                         , eventFormFeedForm            = []
                         , eventFormFeedEvent           = fsEvent
                         , eventFormEventValue          = event
                         , eventFormEventTypes          = []
                         , eventFormAttendance          = psAttend
                         , eventFormDojosAvail          = []
                         , eventFormDetailsEditable     = False
                         , eventFormAttendanceDeletable = False
                         , eventFormCreatedByUser       = Just userCreatedBy
                         , eventFormCreatedByPerson     = Just personCreatedBy }

