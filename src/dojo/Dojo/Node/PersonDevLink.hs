
module Dojo.Node.PersonDevLink (cgiPersonDevLink) where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Chrome
import Dojo.Config
import Dojo.Paths
import Dojo.Fail
import Dojo.Framework
import Dojo.Framework.QRCode
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | Show the device registration QR code for a person.
--      &pid=NAT   Id of the person to show code for.
cgiPersonDevLink
        :: Session -> [(String, String)]
        -> CGI CGIResult

cgiPersonDevLink ss inputs
 | Just strPersonId <- lookup "pid" inputs
 , Right pid    <- parse strPersonId
 = do
        let config = sessionConfig ss
        conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        person  <- liftIO $ getPerson conn pid
        sCode   <- liftIO $ acquirePersonDeviceRegCode conn
                                (configQrSaltActive config) pid
        liftIO $ commit conn
        liftIO $ disconnect conn

        let sName = fromMaybe "[person]" $ personAliasName person
        cgiPageNavi ss "People" sName (pathsJump ss)
         $ H.div ! A.class_ "code-description"
         $ do H.br; H.table $ do
                tr $ td $ H.h3 $ H.string sName
                tr $ td $ H.string "Link to student device registration."
                tr $ td $ H.string ""

                let cc          = sessionConfig ss
                let pRegStatus  = pathPersonDevStatus (sessionConfig ss) sCode
                let sLink       = configSiteUrl cc ++ "/" ++ flatten pRegStatus

                -- Base name of the file to use if the QR code .png image
                -- is downloaded.
                let sCodeFileName
                        = personQRCodeDownloadName person

                -- Generate the QR code inline in the page.
                tr $ td $ htmlQRCode sLink sCodeFileName

                tr $ td $ H.string "The student should scan this code"
                tr $ td $ H.string "and be directed to the page to"
                tr $ td $ H.string "register their own device."
                tr $ td $ H.string $ "code id: " ++ sCode

                tr $ td ! A.style "height:1ex;" $ H.string ""
                tr $ td $ H.string "The direct link is:"
                tr $ td $ (H.a ! A.href (H.toValue sLink)) (H.string sLink)

cgiPersonDevLink _ inputs
 = throw $ FailNodeArgs "person device link" inputs

