
module Dojo.Node.Register (cgiRegister) where
import Dojo.Node.EventEdit.Form
import Dojo.Node.EventEdit.Feed
import Dojo.Node.EventEdit.Arg
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Data.Class
import Dojo.Chrome
import Dojo.Config
import Dojo.Paths
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Data.Time                      as Time


-------------------------------------------------------------------------------
-- | Registration page for specific class.
--     &r=REGID
cgiRegister
 :: Config
 -> [(String, String)]  -- ^ Inputs
 -> String              -- ^ Registration Id.
 -> CGI CGIResult

cgiRegister cc _inputs sRegId
 = do   conn    <- liftIO $ connectSqlite3 $ configDatabasePath cc
        classs  <- liftIO $ getClasses conn

        let sUrl  = configSiteUrl cc
        let sSalt = configQrSaltActive cc

        let lsActive
              =  catMaybes $ flip map classs $ \cls
              -> case registrationLinkOfClass sUrl sSalt cls of
                  Just (_sUrl, sId) -> Just (sId, cls)
                  Nothing           -> Nothing

        case lookup sRegId lsActive of
         Just cls -> cgiRegister_class cc conn sRegId cls
         Nothing  -> cgiRegister_unknown sRegId


cgiRegister_unknown sRegId
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Register"
        H.string $ "registration link unknown " ++ sRegId


cgiRegister_class cc conn sRegId cls
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
         []      -> cgiRegister_create   conn sRegId cls
         [event] -> cgiRegister_existing cc sRegId cls event edate
         events  -> cgiRegister_multi    conn sRegId cls events


-- Make a new empty event record for the given class.
cgiRegister_create _conn sRegId cls
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Register"
        H.string $ "need to make a new event" ++ sRegId ++ " " ++ show cls
        H.br


-- There are already multiple events belonging to the same class,
--   which should only happen if an admin has messed something up.
--   We don't currently have a constraint in the db to prevent this.
cgiRegister_multi _conn sRegId cls events
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Register"
        H.string $ "multiple events exist for the class"
                ++ sRegId ++ " " ++ show cls ++ " " ++ show events


-- There is a single existing event record today for this class.
cgiRegister_existing cc sRegId _cls event _edate
 = outputFPS $ renderHtml
 $ htmlEventRegister cc sRegId [] [] event  []



-------------------------------------------------------------------------------
-- | Html for event registration edit page.
htmlEventRegister
        :: Config -> String
        -> [FeedForm] -> [FeedEvent]
        -> Event -> [Person]
        -> Html

htmlEventRegister cc sRegId fsForm fsEvent event psAttend
 = H.docTypeHtml
 $ do   pageHeader "Event Registration"
        pageBody
         $ H.div ! A.class_ "event"
         $ do   H.h2 "Event Registration"
                formEvent $ EventForm
                 { eventFormPath         = pathRegister cc sRegId
                 , eventFormFeedForm     = fsForm
                 , eventFormFeedEvent    = fsEvent
                 , eventFormEventValue   = event
                 , eventFormEventTypes   = []
                 , eventFormAttendance   = psAttend
                 , eventFormDojosAvail   = [] }

