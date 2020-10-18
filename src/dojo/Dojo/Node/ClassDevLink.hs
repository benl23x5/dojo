
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
         $ H.div ! A.class_ "code-description"
         $ do H.br; H.table $ do
                trClassSummary classs Nothing pOwner
                let cc    = sessionConfig ss
                let sUrl  = configSiteUrl cc
                let sSalt = configQrSaltActive cc

                let Just (sLink, sRegId)
                        = registrationLinkOfClass sUrl sSalt classs

                tr $ td $ htmlQRCode sLink
                tr $ td $ H.string "Scan this code with a registered device"
                tr $ td $ H.string "to join the class attendance list."
                tr $ td $ H.string $ "code id: " ++ sRegId

                tr $ td ! A.style "height:1ex;" $ H.string ""
                tr $ td $ H.string "The direct link is:"
                tr $ td $ (H.a ! A.href (H.toValue sLink)) (H.string sLink)


cgiClassDevLink _ inputs
 = throw $ FailNodeArgs "class device link" inputs

