
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
--   TODO: identify class by hashed registration code, not raw classid.
cgiClassDevReg :: Config -> String -> [(String, String)] -> CGI CGIResult
cgiClassDevReg cc sRegId inputs
 -- ?attend= ... register for class
 | strClassId   <- sRegId
 , Right cid    <- parse strClassId
 , Just "true"  <- lookup "attend" inputs
 = do
        conn    <- liftIO $ connectSqlite3 $ configDatabasePath cc

        -- Class details based on visited URL.
        classs  <- liftIO $ getClass conn cid

        -- Lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner     <- liftIO $ getMaybeUser conn uname
        pOwner          <- liftIO $ getPerson conn $ userPersonId uOwner

        -- Person information based on cookie set in the browser.
        mCookiePerson <- cgiGetPersonOfStudentCookie cc

        case mCookiePerson of
         Nothing        -> throw $ FailNodeArgs "todo unknown person" []
         Just person    -> cgiClassRegister cc conn classs pOwner person

 -- show class registration page.
 | strClassId  <- sRegId
 , Right cid   <- parse strClassId
 = do
        conn    <- liftIO $ connectSqlite3 $ configDatabasePath cc

        -- Class details based on visited URL.
        classs  <- liftIO $ getClass conn cid

        -- Lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner     <- liftIO $ getMaybeUser conn uname
        pOwner          <- liftIO $ getPerson conn $ userPersonId uOwner

        -- Check the week day today, if the class is actually on.
        sDay <- liftIO $ nameOfWeekdayToday

        if | not $ classDay classs == Just (ClassDay sDay)
           -> cgiClassNotToday cc classs pOwner

           | otherwise
           -> do -- Person information based on cookie set in the browser.
                 mCookiePerson <- cgiGetPersonOfStudentCookie cc
                 let bRegistered = False
                 cgiClassStatus cc classs pOwner mCookiePerson bRegistered


cgiClassDevReg _ _ _
 = throw $ FailNodeArgs "class device registration" []


-------------------------------------------------------------------------------
cgiClassStatus
        :: Config
        -> Class -> Person      -- ^ Class details, and class owner.
        -> Maybe Person         -- ^ Person record from device cookie, if any.
        -> Bool                 -- ^ Person is recorded as attending class.
        -> CGI CGIResult

cgiClassStatus cc classs pClassOwner mpCookie bAttending
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

                case mpCookie of
                 Nothing -> do
                   tr $ td $ H.h2 $ (H.p ! A.style "color:Brown;")
                      $ do H.string "Device not Registered "
                           (H.i ! A.class_ "material-icons md-48")
                            $ "sentiment_dissatisfied"

                   tr $ td $ "This device is not registered"
                   tr $ td $ "for the class attendance system."
                   tr $ td $ "Please contact the class instructor."

                 Just pCookie -> do
                   let sName = fromMaybe "[person]" $ personAliasName pCookie
                   tr $ td ! A.style "height:1em;" $ ""
                   tr $ td $ H.h3 $ H.string sName
                   tr $ td ! A.style "height:1ex;" $ H.string ""

                   if bAttending
                    then do
                     tr $ td $ H.h2 $ (H.div ! A.style "color:Green;")
                        $ do H.string "Present "
                             (H.i ! A.class_ "material-icons md-48")
                               $ "sentiment_satisfied_alt"

                     tr $ td ! A.style "height:1ex;" $ H.string ""

                    else H.table $ do
                     tr $ td $ H.h2 $ (H.div ! A.style "color:Brown;")
                        $ do H.string "Not Attending "
                             (H.i ! A.class_ "material-icons md-48")
                               $ "sentiment_dissatisfied"

                     let Just cid  = classId classs
                     let pathClass = pathClassDevReg cc cid
                     tr $ td
                        $ (H.form    ! A.action (fromString $ flatten $ pathClass))
                        $ do H.input ! A.type_ "hidden"
                                     ! A.name  (fromString "r")
                                     ! A.value (fromString $ pretty cid)

                             H.input ! A.type_ "hidden"
                                     ! A.name  (fromString "attend")
                                     ! A.value (fromString "true")

                             H.input ! A.type_ "submit"
                                     ! A.value "Attend Class"


-------------------------------------------------------------------------------
-- | Register a person for a class,
--   creating a new class or adding them to a new one if need be.
cgiClassRegister
        :: IConnection conn
        => Config
        -> conn
        -> Class -> Person
        -> Person
        -> CGI CGIResult

cgiClassRegister cc conn classs pClassOwner _person
 = do
        -- Get all the events for the specified class.
        let Just cid = classId classs
        eventList <- liftIO $ getEventsOfClassId conn cid

        -- See if there is an event created for today already.
        zonedTime <- liftIO $ Time.getZonedTime
        let (edate, _etime)
                = splitEventLocalTime
                $ Time.zonedTimeToLocalTime zonedTime

        -- Events of this class that are on today.
        let eventsToday
                = [ event | (event, _pax) <- eventList
                  , eventDate event == Just edate ]

        -- Get the name of the week day today.
        let localTime = Time.zonedTimeToLocalTime zonedTime
        let (_, nDay) = Time.sundayStartWeek $ Time.localDay localTime
        let sDay      = ssDays !! nDay

        if -- Class is not on today.
           | not $ classDay classs == Just (ClassDay sDay)
           -> cgiClassNotToday cc classs pClassOwner

           -- Class is on today, but there is no event record yet.
           | [] <- eventsToday
           -> goCreate edate

           -- Class is on today, and there is an existing event record
           -- which we can add the person to.
           | [event] <- eventsToday
           -> goExisting event

           -- There are already multiple events belonging to the same class,
           --   which should only happen if an admin has messed something up.
           --   We don't currently have a constraint in the db to prevent this.
           | otherwise
           -> throw $ FailNodeArgs "multiple events exist for class" []


 where
  -- TODO: store this somewhere common
  ssDays :: [String]
         = [ "Sunday", "Monday", "Tuesday"
           , "Wednesday", "Thursday", "Friday", "Saturday" ]

  goCreate _edate
   = throw $ FailNodeArgs "todo gocreate" []

  goExisting _event
   = throw $ FailNodeArgs "todo existing" []


-------------------------------------------------------------------------------
-- | Show a page saying that the class is not on today.
cgiClassNotToday
        :: Config
        -> Class
        -> Person
        -> CGI CGIResult

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

                tr $ td $ H.h2 $ (H.p ! A.style "color:Brown;")
                   $ do H.string "Class not on today "
                        (H.i ! A.class_ "material-icons md-48")
                         $ "sentiment_dissatisfied"


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



