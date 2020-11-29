
module Dojo.Node.Login (cgiLogin) where
import Dojo.Data.Session        ()
import Dojo.Data.User
import Dojo.Data.Session
import Dojo.Framework
import Dojo.Trivia
import Dojo.Config
import Dojo.Chrome
import Dojo.Paths
import Data.Word
import Data.String
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified System.Random                  as R
import qualified Network.CGI                    as CGI
import qualified Data.Digest.Pure.MD5           as Digest
import qualified Data.ByteString.Lazy           as BS
import qualified Data.Time                      as Time


-------------------------------------------------------------------------------
-- | The entry login page
cgiLogin :: Config -> [(String, String)] -> CGI CGIResult
cgiLogin cc inputs

 -- We have the username and password
 | Just username <- lookup "username" inputs
 , Just password <- lookup "password" inputs
 = do
        conn    <- liftIO $ connectSqlite3 $ configPathDatabase cc
        mUser   <- liftIO $ getMaybeUser conn (UserName username)
        liftIO $ disconnect conn

        -- Check if there is a user with this name.
        case mUser of
         Nothing    -> loginFail cc
         Just user  -> loginCheck cc user password

 -- No username/password yet, so display login page.
 | otherwise
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Login"
        pageBody
         $ H.div ! A.id "login-page"
         $ do
                -- Set the image size so that works on phone form factors
                -- as well as in a full screen laptop browser. If the logo
                -- is any larger than this it doesn't fit on the page in the
                -- laptop form factor.
                H.img
                 ! (A.src $ fromString $ configLogoUrl cc)
                 ! (A.style "width:450px;height:450px")
                 ! (A.id    "login-logo")

                H.div ! A.class_ "login-entry"
                 $ do   formLogin (pathLogin cc)


formLogin :: Path -> Html
formLogin path
 = H.form ! A.action (H.toValue path)
 $ H.table
 $ do
        tr $ do (td ! A.class_ "login-icon")
                 $ (H.i ! A.class_ "material-icons md-36")
                 $ "face"

                td $ input ! A.name  "username"
                           ! A.type_ "text"
                           ! A.autocomplete "off"
                           ! A.autofocus    "on"

        tr $ do (td ! A.class_ "login-icon")
                 $ (H.i ! A.class_ "material-icons md-36")
                 $ "vpn_key"

                td $ input ! A.name  "password"
                           ! A.type_ "password"

        tr $ (td ! A.colspan "2")
           $ input ! A.type_  "submit"
                   ! A.class_ "button-login"
                   ! A.value  "Login"


-------------------------------------------------------------------------------
-- | Check user password.
loginCheck :: Config -> User -> String -> CGI CGIResult
loginCheck cc user password
 = do
        let UserPasswordHash hash = userPasswordHash user
        let UserPasswordSalt salt = userPasswordSalt user

        -- Combine the supplied password with the salt to get
        -- the hash we need. We accept a newline on the end so it's
        -- easy to make these hashes at the unix console.
        let hash' = Digest.md5
                  $ BS.pack $ map convert $ password ++ salt ++ "\n"

        -- Whether the entered password matches the stored one.
        let match = hash == show hash'

        if not match
         then loginFail cc
         else loginActivate cc user


-- | Login failure, bad username or password.
loginFail :: Config -> CGI CGIResult
loginFail cc
 = CGI.redirect $ flatten
 $ pathDebug cc "Login failed."


-- | Activate user session and go to main page.
loginActivate :: Config -> User -> CGI CGIResult
loginActivate cc user
 = do
        -- Make a session key based on a random number.
        gen :: Word64 <- liftIO $ R.randomIO
        let hash' = take 12 $ show
                  $ Digest.md5 $ BS.pack $ map convert $ show gen

        let hash  = SessionHash hash'

        -- Use the current time as a placeholder until we have
        -- the real time.
        zonedTime <- liftIO $ Time.getZonedTime
        let (startDate, startTime)
                = splitSessionLocalTime
                $ Time.zonedTimeToLocalTime zonedTime

        let session
                = Session
                { sessionConfig         = cc
                , sessionId             = SessionId 0
                , sessionHash           = hash
                , sessionUserId         = userId user
                , sessionRoleNative     = userRoleNative user
                , sessionRoleActive     = userRoleNative user
                , sessionStartDate      = startDate
                , sessionStartTime      = startTime
                , sessionEndDate        = Nothing
                , sessionEndTime        = Nothing }

        -- Connect to database.
        conn    <- liftIO $ connectSqlite3 $ configPathDatabase cc

        -- Insert the new session
        liftIO $ insertSession conn session

        -- Commit and disconnect from database.
        liftIO  $ commit conn
        liftIO  $ disconnect conn

        -- Redirect to main page.
        CGI.redirect $ flatten $ (pathClassList session)
