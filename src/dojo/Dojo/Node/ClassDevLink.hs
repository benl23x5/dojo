
module Dojo.Node.ClassDevLink where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.User
import Dojo.Data.Class
import Dojo.Node.ClassView
import Dojo.Paths
import Dojo.Chrome
import Dojo.Config
import Dojo.Fail
import Dojo.Framework
import Dojo.Framework.QRCode
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | Class device registration.
cgiClassDevLink :: Session -> [(String, String)] -> CGI CGIResult
cgiClassDevLink ss inputs
 | Just strClassId  <- lookup "cid" inputs
 , Right cid        <- parse strClassId
 = do
        conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        classs  <- liftIO $ getClass conn cid

        -- Lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner     <- liftIO $ getMaybeUser conn uname
        pOwner          <- liftIO $ getPerson conn $ userPersonId uOwner

        cgiPageNavi ss "Classes" (classDisplayName classs) (pathsJump ss)
         $ H.div ! A.class_ "class-device-reg"
         $ do
                -- Only bother showing user id for class owner to non-admins.
                let muOwner
                        = if sessionIsAdmin ss
                                then Just uOwner
                                else Nothing

                H.table $ trClassSummary classs muOwner pOwner
                tableActions [ pathClassView ss cid ]

                let cc    = sessionConfig ss
                let pReg  = pathClassDevReg (sessionConfig ss) cid
                let sLink = configSiteUrl cc ++ "/" ++ flatten pReg
                htmlQRCode sLink

                H.div ! A.class_ "code-description"
                 $ H.table
                 $ do   tr $ td $ H.string "Scan this code with a registered device"
                        tr $ td $ H.string "to join the class attendance list."


cgiClassDevLink _ inputs
 = throw $ FailNodeArgs "class device link" inputs

