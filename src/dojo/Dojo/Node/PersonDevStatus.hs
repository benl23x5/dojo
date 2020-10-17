
module Dojo.Node.PersonDevStatus
        ( cgiPersonDevStatus
        , cgiGetPersonOfStudentCookie)
where
import Dojo.Data.Person
import Dojo.Chrome
import Dojo.Config
import Dojo.Fail
import Dojo.Paths
import Dojo.Framework
import Data.String
import qualified Data.Char                      as Char
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Network.CGI                    as CGI

-- Name of the student identification cookie.
-- TODO: get this from the config.
sCookieStudent
 = "aikikai-australia-student"

-- TODO: log registration / unregistration events.

-------------------------------------------------------------------------------
-- | Show the device registration QR code for a person.
--      &pid=NAT   Id of the person to show code for.
cgiPersonDevStatus
        :: Config
        -> [(String, String)]   -- inputs
        -> String
        -> CGI CGIResult

cgiPersonDevStatus cc inputs strPersonId
 -- register device
 | Right pid    <- parse strPersonId
 , Just _       <- lookup "register" inputs
 = do
        cgiSetStudentCookie pid
        redirect $ flatten $ pathPersonDevStatus cc pid


 -- unregister device
 | Right pid    <- parse strPersonId
 , Just _       <- lookup "unregister" inputs
 = do
        -- template of cookie to delete.
        -- only the 'name', 'domain' and 'path' are used.
        let cookieParts
                = CGI.Cookie
                { CGI.cookieName        = sCookieStudent
                , CGI.cookieValue       = ""
                , CGI.cookieExpires     = Nothing
                , CGI.cookieDomain      = Just "ouroborus.net"  -- TODO: get from config.
                , CGI.cookiePath        = Nothing
                , CGI.cookieSecure      = True
                , CGI.cookieHttpOnly    = True }

        CGI.deleteCookie cookieParts
        redirect $ flatten $ pathPersonDevStatus cc pid


 -- show status page
 | Right pid    <- parse strPersonId
 = do   conn    <- liftIO $ connectSqlite3 $ configDatabasePath cc

        -- Person information based on the visited url.
        person  <- liftIO $ getPerson conn pid
        liftIO $ disconnect conn

        -- Person information based on any set cookie.
        mCookiePerson <- cgiGetPersonOfStudentCookie cc

        cgiShowStatus cc pid person (isJust mCookiePerson)

cgiPersonDevStatus _ _ _
 = throw $ FailNodeArgs "person device status" []


-------------------------------------------------------------------------------
cgiShowStatus :: Config -> PersonId -> Person -> Bool -> CGI CGIResult
cgiShowStatus cc pid person bRegistered
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
         $ do   tr $ td $ H.h3 $ H.string sName
                tr $ td $ H.string "Student Device Registration"

        let pathStatus = pathPersonDevStatus cc pid
        H.div ! A.class_ "person-device-register"
         $ if bRegistered
            then H.table $ do
                tr $ td $ H.h2 $ (H.p ! A.style "color:Green;")
                   $ do H.string "Registered "
                        (H.i ! A.class_ "material-icons md-48")
                          $ "sentiment_satisfied_alt"

                H.tr $ H.td
                   $ (H.form    ! A.action (fromString $ flatten $ pathStatus))
                   $ do H.input ! A.type_ "hidden"
                                ! A.name  (fromString "pds")
                                ! A.value (fromString $ pretty pid)

                        H.input ! A.type_ "hidden"
                                ! A.name (fromString "unregister")

                        H.input ! A.type_ "submit"
                                ! A.value "Unregister Device"


            else H.table $ do
                tr $ td $ H.h2 $ (H.p ! A.style "color:Brown;")
                   $ do H.string "Not Registered "
                        (H.i ! A.class_ "material-icons md-48")
                         $ "sentiment_dissatisfied"

                tr $ td
                   $ (H.form    ! A.action (fromString $ flatten $ pathStatus))
                   $ do H.input ! A.type_ "hidden"
                                ! A.name  (fromString "pds")
                                ! A.value (fromString $ pretty pid)

                        H.input ! A.type_ "hidden"
                                ! A.name (fromString "register")

                        H.input ! A.type_ "submit"
                                ! A.value "Register Device"


-------------------------------------------------------------------------------
-- | Set the student device registration cookie.
cgiSetStudentCookie :: PersonId -> CGI ()
cgiSetStudentCookie pid
 = do   let PersonId iPid = pid
        let cookie
                = CGI.Cookie
                { CGI.cookieName        = sCookieStudent
                , CGI.cookieValue       = show iPid
                , CGI.cookieExpires     = Nothing               -- TODO: set ~6m in future.
                , CGI.cookieDomain      = Just "ouroborus.net"  -- TODO: get from config.
                , CGI.cookiePath        = Nothing
                , CGI.cookieSecure      = True
                , CGI.cookieHttpOnly    = True }

        CGI.setCookie cookie


-------------------------------------------------------------------------------
-- | Get the student device registraton cookie, if there is one.
cgiGetPersonOfStudentCookie :: Config -> CGI (Maybe Person)
cgiGetPersonOfStudentCookie cc
 = do   mValue  <- CGI.getCookie sCookieStudent
        case mValue of
         Nothing -> return Nothing
         Just sPid
          |  all Char.isDigit sPid
          ,  iPid <- read sPid
          -> do conn   <- liftIO $ connectSqlite3 $ configDatabasePath cc
                person <- liftIO $ getPerson conn (PersonId iPid)
                return $ Just person

          | otherwise
          -> return Nothing


