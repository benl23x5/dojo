
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

                -- Link to the person specific device registration page.
                let Just (sLink, sRegId)
                        = registrationLinkOfClass sUrl sSalt classs

                -- Base name of the file to use if the QR code .png
                -- image is downloaded.
                let sCodeFileName
                        = classQRCodeDownloadName classs

                H.tr $ H.td $ htmlQRCode sLink sCodeFileName
                H.tr $ H.td $ H.string "Scan this code with a registered device"
                H.tr $ H.td $ H.string "to join the class attendance list."
                H.tr $ H.td $ H.string $ "code id: " ++ sRegId

                H.tr $ H.td ! A.style "height:1ex;" $ H.string ""
                H.tr $ H.td $ H.string "The direct link is:"
                H.tr $ H.td $ (H.a ! A.href (H.toValue sLink)) (H.string sLink)


cgiClassDevLink _ inputs
 = throw $ FailNodeArgs "class device link" inputs

