
module Dojo.Node.PersonDevStatus
        ( cgiPersonDevStatus
        , cgiGetPersonOfStudentCookie)
where
import Dojo.Data.Person
import Dojo.Chrome
import Dojo.Config
import Dojo.Paths
import Dojo.Framework
import Data.String
import qualified Network.CGI                    as CGI
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Data.Time.Clock                as Time


-------------------------------------------------------------------------------
-- | Show the device registration QR code for a person.
--      &pid=NAT   Id of the person to show code for.
cgiPersonDevStatus
        :: Config
        -> [(String, String)]   -- inputs
        -> String
        -> CGI CGIResult

cgiPersonDevStatus cc inputs sCode
 -- Register device by setting the cookie.
 | Just _       <- lookup "register" inputs
 = do   conn    <- liftIO $ connectSqlite3 $ configPathDatabase cc
        mpid    <- liftIO $ lookupPersonIdOfDeviceRegCode conn sCode
        case mpid of
         Nothing
          ->    cgiPersonDevRegUnrecognizedClassCode cc

         Just pid
          -> do cgiSetStudentCookie cc conn pid
                redirect $ flatten $ pathPersonDevStatus cc sCode

 -- Unregister device by clearing the cookie.
 | Just _ <- lookup "unregister" inputs
 = do
        -- template of cookie to delete.
        -- only the 'name', 'domain' and 'path' are used.
        let cookieParts
                = CGI.Cookie
                { CGI.cookieName        = configCookieNameStudentReg cc
                , CGI.cookieValue       = ""
                , CGI.cookieExpires     = Nothing
                , CGI.cookieDomain      = Just $ configCookieDomain cc
                , CGI.cookiePath        = Nothing
                , CGI.cookieSecure      = True
                , CGI.cookieHttpOnly    = True }

        CGI.deleteCookie cookieParts
        redirect $ flatten $ pathPersonDevStatus cc sCode

 -- show status page
 | otherwise
 = do   conn <- liftIO $ connectSqlite3 $ configPathDatabase cc
        mpid <- liftIO $ lookupPersonIdOfDeviceRegCode conn sCode
        case mpid of
         Nothing
          -> cgiPersonDevRegUnrecognizedClassCode cc

         Just pid
          -> do person  <- liftIO $ getPerson conn pid

                -- Person information based on any set cookie.
                mCookiePerson <- cgiGetPersonOfStudentCookie cc conn
                liftIO $ disconnect conn

                -- TODO: compare page pid with the set cookie.
                -- If they are for different people then display "unregistered"
                -- relative to the page pid, not relative to the existing cookie.
                cgiShowStatus cc sCode person (isJust mCookiePerson)


-------------------------------------------------------------------------------
-- | Set the student device registration cookie.
--
--   TODO: set the cookie as the student reg code, not the pid.
--   TODO: refresh the cookie after each event registration.
--
cgiSetStudentCookie
        :: IConnection conn
        => Config -> conn -> PersonId -> CGI ()

cgiSetStudentCookie cc conn pid
 = do
        -- Acquire the current reg code for the person,
        --  either by using the existing one recorded in the database,
        --  or generating a fresh one.
        sCode  <- liftIO $ acquirePersonDeviceRegCode
                        conn (configQrSaltActive cc) pid
        liftIO $ commit conn

        -- Set the cookie expiry 6 months in the future.
        --
        --  If the person has not used the code to register for 6 months,
        --  then maybe they've had a break in training and the reg. system
        --  has changed. See the instructor anyway.
        --
        --  Each time the student visits a class registration page
        --  the expiry date will be shifted into the future.
        --
        utcNow <- liftIO $ Time.getCurrentTime
        let utcLater = Time.addUTCTime (Time.nominalDay * 185) utcNow

        let cookie
                = CGI.Cookie
                { CGI.cookieName        = configCookieNameStudentReg cc
                , CGI.cookieValue       = sCode
                , CGI.cookieExpires     = Just $ utcLater
                , CGI.cookieDomain      = Just $ configCookieDomain cc
                , CGI.cookiePath        = Nothing
                , CGI.cookieSecure      = True
                , CGI.cookieHttpOnly    = True }

        CGI.setCookie cookie


-------------------------------------------------------------------------------
-- | Get the student device registraton cookie, if there is one.
cgiGetPersonOfStudentCookie
        :: IConnection conn
        => Config -> conn -> CGI (Maybe Person)

cgiGetPersonOfStudentCookie cc conn
 = do   mValue  <- CGI.getCookie $ configCookieNameStudentReg cc
        case mValue of
         Nothing
          -> return Nothing

         Just sCode
          -> do mpid <- liftIO $ lookupPersonIdOfDeviceRegCode conn sCode
                case mpid of
                 Just pid
                  -> do -- Lookup the person record based on the pid.
                        person <- liftIO $ getPerson conn pid

                        -- Reset the cookie to move the expiry date
                        -- into the future.
                        cgiSetStudentCookie cc conn pid

                        return $ Just person

                 _ -> return Nothing


-------------------------------------------------------------------------------
cgiShowStatus :: Config -> String -> Person -> Bool -> CGI CGIResult
cgiShowStatus cc sCode person bRegistered
 = cgiPagePlain "Device Status"
 $ H.div ! A.class_ "code-description"
 $ do
        H.img
         ! (A.src $ fromString $ configLogoUrl cc)
         ! (A.style "width:450px;height:450px")
         ! (A.id    "login-logo")

        H.br

        let sName = fromMaybe "[person]" $ personAliasName person
        H.div ! A.class_ "person-alias-name"
         $ H.table
         $ do   H.tr $ H.td $ H.h3 $ H.string sName
                H.tr $ H.td $ H.string "Student Device Registration"

        let pathStatus = pathPersonDevStatus cc sCode
        H.div ! A.class_ "person-device-register"
         $ if bRegistered
            then H.table $ do
                H.tr $ H.td $ H.h2 $ (H.p ! A.style "color:Green;")
                   $ do H.string "Registered "
                        (H.i ! A.class_ "material-icons md-48")
                          $ "sentiment_satisfied_alt"

                H.tr $ H.td
                   $ (H.form    ! A.action (fromString $ flatten $ pathStatus))
                   $ do
                        H.input ! A.type_ "hidden"
                                ! A.name  (fromString "pds")
                                ! A.value (fromString sCode)

                        H.input ! A.type_ "hidden"
                                ! A.name (fromString "unregister")

                        H.input ! A.type_ "submit"
                                ! A.value "Unregister Device"


            else H.table $ do
                H.tr $ H.td $ H.h2 $ (H.p ! A.style "color:Brown;")
                   $ do H.string "Not Registered "
                        (H.i ! A.class_ "material-icons md-48")
                         $ "sentiment_dissatisfied"

                H.tr $ H.td
                   $ (H.form    ! A.action (fromString $ flatten $ pathStatus))
                   $ do
                        H.input ! A.type_ "hidden"
                                ! A.name  (fromString "pds")
                                ! A.value (fromString sCode)

                        H.input ! A.type_ "hidden"
                                ! A.name (fromString "register")

                        H.input ! A.type_ "submit"
                                ! A.value "Register Device"



-------------------------------------------------------------------------------
-- | Page saying we don't recognize the class registration code.
cgiPersonDevRegUnrecognizedClassCode :: Config -> CGI CGIResult
cgiPersonDevRegUnrecognizedClassCode cc
 = cgiPagePlain "Unrecognized Person Code"
 $ H.div ! A.class_ "person-dev-reg"
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
