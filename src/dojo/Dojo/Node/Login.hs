
module Dojo.Node.Login
        (cgiLogin)
where
import Dojo.Data.Session        ()
import Dojo.Data.User
import Dojo.Data.Session
import Dojo.Chrome
import Dojo.Framework
import Dojo.Paths
import qualified Config
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified System.Random                  as R
import qualified Network.CGI                    as CGI
import qualified Data.Digest.Pure.MD5           as Digest
import qualified Data.ByteString.Lazy           as BS
import qualified Data.Time                      as Time


-- The entry login page
cgiLogin :: [(String, String)] -> CGI CGIResult
cgiLogin inputs

 -- Have username and password
 | Just username <- lookup "username" inputs
 , Just password <- lookup "password" inputs
 = do
        -- Connect to database.
        conn    <- liftIO $ connectSqlite3 Config.databasePath

        -- Try to get the user record with this name.
        mUser   <- liftIO $ getMaybeUser conn (UserName username)

        -- Disconnect from database.
        liftIO $ disconnect conn

        case mUser of
         -- No user with this name
         Nothing        -> loginFail

         -- Have a user with this name, check the password.
         Just user      -> loginCheck user password

 -- Display login page.
 | otherwise
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Login"
        pageBody
         $ do   H.h1 $ H.string $ Config.siteName
                formLogin pathLogin


-------------------------------------------------------------------------------
formLogin :: Path -> Html
formLogin path
 = H.div ! A.id "login"
 $ form  ! A.action (H.toValue path)
 $ do
        mapM_   (\(fieldName, fieldData)
                 -> input ! A.type_ "hidden"
                          ! A.name  (H.toValue fieldName)
                          ! A.value (H.toValue fieldData))
                (pathFields path)

        H.table
         $ do   tr $ do
                        td "Username:"
                        td $ input ! A.name  "username"
                                   ! A.type_ "text"
                                   ! A.autocomplete "off"
                                   ! A.autofocus    "on"

                tr $ do td "Password:"
                        td $ input ! A.name  "password"
                                   ! A.type_ "password"

        H.br
        input   ! A.type_  "submit"
                ! A.class_ "buttonFull"
                ! A.value  "Login"


-------------------------------------------------------------------------------
-- | Check user password.
loginCheck :: User -> String -> CGI CGIResult
loginCheck user password
 = do
        let UserPasswordHash hash = userPasswordHash user
        let UserPasswordSalt salt = userPasswordSalt user

        -- Combine the supplied password with the salt to get
        -- the hash we need. We accept a newline on the end so it's
        -- easy to make these hashes at the unix console.
        let hash'       = Digest.md5
                        $ BS.pack $ map convert $ password ++ salt ++ "\n"

        -- Whether the entered password matches the stored one.
        let match       = hash == show hash'

        if not match
         then loginFail
         else loginActivate user


-- | Login failure, bad username or password.
loginFail :: CGI CGIResult
loginFail
        = CGI.redirect $ flatten $ pathLogin


-- | Activate user session and go to main page.
loginActivate :: User -> CGI CGIResult
loginActivate user
 = do
        -- Make a session key based on a random number.
        gen :: Int      <- liftIO $ R.randomRIO (0 :: Int, 2^ (30 :: Int) :: Int)
        let hash'       =  take 12
                        $  show
                        $  Digest.md5
                        $  BS.pack $ map convert $ show gen

        let hash        = SessionHash hash'

        -- Use the current time as a placeholder until we have
        -- the real time.
        zonedTime       <- liftIO $ Time.getZonedTime
        let (startDate, startTime)
                        = splitSessionLocalTime
                        $ Time.zonedTimeToLocalTime zonedTime

        let session     = makeSession
                                hash
                                (userId user)
                                startDate
                                startTime

        -- Connect to database.
        conn    <- liftIO $ connectSqlite3 $ Config.databasePath

        -- Insert the new session
        _       <- liftIO $ insertSession conn session

        -- Commit and disconnect from database.
        liftIO  $ commit conn
        liftIO  $ disconnect conn

        -- Redirect to main page.
        CGI.redirect $ flatten $ (pathEventList session)



