
module Dojo.Node.ClassDevReg where
import Dojo.Data
import Dojo.Node.ClassView
import Dojo.Node.PersonDevStatus
import Dojo.Chrome
import Dojo.Fail
import Dojo.Config
import Dojo.Framework
import Dojo.Paths
import Data.String
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Data.Time                      as Time
import qualified Data.Time.Calendar.OrdinalDate as Time


-------------------------------------------------------------------------------
-- | Class device registration.
cgiClassDevReg :: Config -> String -> [(String, String)] -> CGI CGIResult
cgiClassDevReg cc sRegId inputs
 = do
        -- Connect to database.
        conn    <- liftIO $ connectSqlite3 $ configPathDatabase cc

        -- Get list of all active classes.
        classes <- liftIO $ getClasses conn

        let sUrl  = configSiteUrl cc
        let sSalt = configQrSaltActive cc

        -- Get registration links of all active classes.
        let lsActive
              =  catMaybes $ flip map classes $ \cls
              -> case registrationLinkOfClass sUrl sSalt cls of
                  Just (_sUrl, sId) -> Just (sId, cls)
                  Nothing           -> Nothing

        -- Lookup the registration link.
        case lookup sRegId lsActive of
         Just classs -> cgiClassDevRegMatch cc conn inputs classs
         Nothing     -> cgiClassDevRegUnrecognizedClassCode cc


-------------------------------------------------------------------------------
-- | Class device registration,
--   where we have a class record based on a matching registration id.
cgiClassDevRegMatch
        :: IConnection conn
        => Config -> conn -> [(String, String)]
        -> Class  -> CGI CGIResult

cgiClassDevRegMatch cc conn inputs classs
 -- ?attend= ... register for class
 | Just "true"  <- lookup "attend" inputs
 = do
        -- Lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner <- liftIO $ getMaybeUser conn uname
        pOwner      <- liftIO $ getPerson conn $ userPersonId uOwner

        -- Person information based on cookie set in the browser.
        mCookiePerson <- cgiGetPersonOfStudentCookie cc conn
        case mCookiePerson of
         Nothing     -> cgiClassDevRegUnrecognizedDevice cc classs pOwner
         Just person -> cgiClassDevRegAddPerson cc conn classs uOwner pOwner person

 -- Show class registration page.
cgiClassDevRegMatch cc conn _inputs classs
 = do
        let Just cid = classId classs

        -- Lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner <- liftIO $ getMaybeUser conn uname
        pOwner      <- liftIO $ getPerson conn $ userPersonId uOwner

        -- Check the week day today, if the class is actually on.
        sDay <- liftIO $ nameOfWeekdayToday

        -- All events that are on today.
        eventsToday <- liftIO $ getEventsOfClassToday conn cid

        -- Person information based on cookie set in the browser.
        mCookiePerson <- cgiGetPersonOfStudentCookie cc conn

        if -- This class is not on today.
           | not $ classDay classs == Just (ClassDay sDay)
           -> cgiClassNotToday cc classs pOwner

           -- There is already an event for today,
           -- and the person might already be registered for it.
           | Just person <- mCookiePerson
           -> do  let Just pid = personId person

                  -- All people that are currently listed as attenees.
                  psAttend
                   <- case eventsToday of
                        []      -> return []
                        [event] -> do
                                let Just eid = eventId event
                                liftIO $ getAttendance conn eid
                        _ -> throw $ FailNodeArgs "multiple events exist for class" []

                  -- Whether the current person is listed as an attendee.
                  let bAttending = elem pid $ mapMaybe personId psAttend

                  -- Show the main status page.
                  cgiClassDevRegStatus cc classs pOwner person bAttending

           -- The device accessing the page is not recognized.
           -- No student identification cookie is set.
           | otherwise
           -> cgiClassDevRegUnrecognizedDevice cc classs pOwner


-------------------------------------------------------------------------------
-- | Page saying we don't recognize the class registration code.
cgiClassDevRegUnrecognizedClassCode
        :: Config
        -> CGI CGIResult

cgiClassDevRegUnrecognizedClassCode cc
 = cgiPagePlain "Unrecognized Class Code"
 $ H.div ! A.class_ "class-dev-reg"
 $ do
        -- TODO: shfit logo style definition to chrome module
        -- don't keep repeating the html.
        H.img
         ! (A.src $ fromString $ configLogoUrl cc)
         ! (A.style "width:450px;height:450px")
         ! (A.id    "login-logo")

        H.br
        H.div ! A.class_ "code-description"
         $ H.table $ do
                H.tr $ H.td $ H.h2 $ (H.p ! A.style "color:Brown;")
                   $ do H.string "Code Not Recognized "
                        (H.i ! A.class_ "material-icons md-48")
                         $ "sentiment_dissatisfied"

                H.tr $ H.td $ "This registration code is not recognized."
                H.tr $ H.td $ "Please contact the class instructor."


-------------------------------------------------------------------------------
-- | Page saying we don't recognize the student device.
cgiClassDevRegUnrecognizedDevice
        :: Config
        -> Class                -- ^ Class they tried to register for.
        -> Person               -- ^ Owner of class.
        -> CGI CGIResult

cgiClassDevRegUnrecognizedDevice cc classs pClassOwner
 = cgiPagePlain (classDisplayName classs)
 $ H.div ! A.class_ "class-dev-reg"
 $ do
        H.img
         ! (A.src $ fromString $ configLogoUrl cc)
         ! (A.style "width:450px;height:450px")
         ! (A.id    "login-logo")

        H.br
        H.div ! A.class_ "code-description"
         $ H.table $ do
                trClassSummary classs Nothing pClassOwner

                H.tr $ H.td $ H.h2 $ (H.p ! A.style "color:Brown;")
                   $ do H.string "Device not Registered "
                        (H.i ! A.class_ "material-icons md-48")
                         $ "sentiment_dissatisfied"

                H.tr $ H.td $ "This device is not registered"
                H.tr $ H.td $ "for the class attendance system."
                H.tr $ H.td $ "Please contact the class instructor."


-------------------------------------------------------------------------------
-- | Try to register a person for a class,
--   creating a new class or adding them to a new one if need be.
cgiClassDevRegAddPerson
        :: IConnection conn
        => Config -> conn
        -> Class -> User -> Person  -- ^ Class details, and class owner.
        -> Person                   -- ^ Person to register for the class.
        -> CGI CGIResult

cgiClassDevRegAddPerson cc conn classs uClassOwner pClassOwner person
 = do
        -- Get all the events for the specified class.
        let Just cid = classId classs

        -- See if there is an event created for today already.
        zonedTime <- liftIO $ Time.getZonedTime
        let (edate, _etime)
                = splitEventLocalTime
                $ Time.zonedTimeToLocalTime zonedTime

        -- Events of this class that are on today.
        eventsToday <- liftIO $ getEventsOfClassToday conn cid

        -- Get the name of the week day today.
        sDay <- liftIO nameOfWeekdayToday

        if -- Class is not on today.
           | not $ classDay classs == Just (ClassDay sDay)
           -> cgiClassNotToday cc classs pClassOwner

           -- Class is on today, but there is no event record yet.
           -- This is the first person registering for it,
           -- so we need to create the event record first.
           | [] <- eventsToday
           -> cgiClassDevRegCreateEvent
                cc conn classs uClassOwner edate person

           -- Class is on today, and there is an existing event record
           -- which we can add the person to.
           | [event] <- eventsToday
           -> cgiClassDevRegExistingEvent
                cc conn classs person event

           -- There are already multiple events belonging to the same class,
           --  which should only happen if an admin has messed something up.
           --  We don't have a constraint in the db to prevent this.
           | otherwise
           -> throw $ FailNodeArgs "multiple events exist for class" []


-------------------------------------------------------------------------------
-- | Show current attendance status for the class,
--   whether or not a registered student is listed as attending the class.
cgiClassDevRegStatus
        :: Config
        -> Class -> Person  -- ^ Class details, and class owner.
        -> Person           -- ^ Person record from device cookie
        -> Bool             -- ^ Person is recorded as attending class.
        -> CGI CGIResult

cgiClassDevRegStatus cc classs pClassOwner pCookie bAttending
 = cgiPagePlain (classDisplayName classs)
 $ H.div ! A.class_ "class-dev-reg"
 $ do
        H.img
         ! (A.src $ fromString $ configLogoUrl cc)
         ! (A.style "width:450px;height:450px")
         ! (A.id    "login-logo")

        H.br
        H.div ! A.class_ "code-description"
         $ H.table
         $ do   trClassSummary classs Nothing pClassOwner

                let sName = fromMaybe "[person]" $ personAliasName pCookie
                H.tr $ H.td ! A.style "height:1em;" $ ""
                H.tr $ H.td $ H.h3 $ H.string sName
                H.tr $ H.td ! A.style "height:1ex;" $ H.string ""

                if bAttending
                 then do
                  -- Attending
                  H.tr $ H.td $ H.h2 $ (H.div ! A.style "color:Green;")
                     $ do H.string "Attending "
                          (H.i ! A.class_ "material-icons md-48")
                            $ "sentiment_satisfied_alt"

                  H.tr $ H.td ! A.style "height:1ex;" $ H.string ""

                 else do
                  -- Not attending
                  H.tr $ H.td $ H.h2 $ (H.div ! A.style "color:Brown;")
                     $ do H.string "Not Attending "
                          (H.i ! A.class_ "material-icons md-48")
                            $ "sentiment_dissatisfied"

                  let sUrl  = configSiteUrl cc
                  let sSalt = configQrSaltActive cc
                  let Just (sRegLink, sRegId)
                        = registrationLinkOfClass sUrl sSalt classs

                  -- Button to record attendance in the class.
                  H.tr $ H.td
                     $ (H.form    ! A.action (fromString sRegLink))
                     $ do H.input ! A.type_ "hidden"
                                  ! A.name  (fromString "r")
                                  ! A.value (fromString sRegId)

                          H.input ! A.type_ "hidden"
                                  ! A.name  (fromString "attend")
                                  ! A.value (fromString "true")

                          H.input ! A.type_ "submit"
                                  ! A.value "Attend Class"


-------------------------------------------------------------------------------
-- | Show a page saying that the class is not on today.
cgiClassNotToday :: Config -> Class -> Person -> CGI CGIResult
cgiClassNotToday cc classs pClassOwner
 = cgiPagePlain (classDisplayName classs)
 $ H.div ! A.class_ "class-dev-reg"
 $ do
        H.img
         ! (A.src $ fromString $ configLogoUrl cc)
         ! (A.style "width:450px;height:450px")
         ! (A.id    "login-logo")

        H.br
        H.div ! A.class_ "code-description"
         $ H.table
         $ do   trClassSummary classs Nothing pClassOwner

                H.tr $ H.td $ H.h2 $ (H.p ! A.style "color:Brown;")
                   $ do H.string "Class not on today "
                        (H.i ! A.class_ "material-icons md-48")
                         $ "sentiment_dissatisfied"


-------------------------------------------------------------------------------
-- | Create a new event for a class and add a person to it.
--   The person is the first one that is registering for the class today.
cgiClassDevRegCreateEvent
        :: IConnection conn
        => Config -> conn
        -> Class -> User        -- ^ Class and its owning user.
        -> EventDate            -- ^ Date to create the new event.
        -> Person               -- ^ Person to add to freshly created event.
        -> CGI CGIResult

cgiClassDevRegCreateEvent cc conn classs uOwner edate person
 = do
        let event = zeroEvent
                  { eventType           = classType classs
                  , eventLocation       = classLocation classs
                  , eventDate           = Just edate
                  , eventTime           = classTimeStart classs
                  , eventCreatedBy      = Just $ userId uOwner }

        event' <- liftIO $ insertEvent conn event
        cgiClassDevRegExistingEvent cc conn classs person event'


-------------------------------------------------------------------------------
-- | Add a person as an attendee to an existing event,
--   then redirect to their attendance status page.
cgiClassDevRegExistingEvent
        :: IConnection conn
        => Config -> conn
        -> Class                -- ^ Class to attend.
        -> Person               -- ^ Person that is attending the class.
        -> Event                -- ^ Event record for the person to join.
        -> CGI CGIResult

cgiClassDevRegExistingEvent cc conn classs person event
 = do
        let Just pid = personId person
        let Just eid = eventId event

        -- People that are already attending the event.
        psAttend <- liftIO $ getAttendance conn eid
        let pidsAttend  = mapMaybe personId psAttend

        if -- Person is already listed as an attendee,
           -- so just to back to the main reg. status page.
           |  elem pid pidsAttend
           -> do liftIO $ disconnect conn
                 redirect $ flatten $ pathClassDevReg cc classs

           | otherwise
           -> do liftIO $ insertAttendance conn eid person
                 liftIO $ commit conn
                 liftIO $ disconnect conn
                 redirect $ flatten $ pathClassDevReg cc classs


-------------------------------------------------------------------------------
-- | Get the name of the weekday today.
nameOfWeekdayToday :: IO String
nameOfWeekdayToday
 = do   zonedTime <- liftIO $ Time.getZonedTime
        let localTime = Time.zonedTimeToLocalTime zonedTime
        let (_, nDay) = Time.sundayStartWeek $ Time.localDay localTime

        let ssDays :: [String]
                = [ "Sunday", "Monday", "Tuesday"
                  , "Wednesday", "Thursday", "Friday", "Saturday" ]

        return  $ ssDays !! nDay
