
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
 = do   conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        person  <- liftIO $ getPerson conn pid
        liftIO $ disconnect conn

        let sName = fromMaybe "[person]" $ personAliasName person
        cgiPageNavi ss "Device Registration" sName (pathsJump ss)
         $ H.div ! A.class_ "person-device-reg"
         $ do
                H.br
                H.div ! A.class_ "person-alias-name"
                 $ H.table
                 $ do   tr $ td $ H.string sName
                        tr $ td $ H.string "Link to student device registration."
                        tr $ td $ H.string ""

                let cc          = sessionConfig ss
                let pRegStatus  = pathPersonDevStatus ss pid
                let sLink       = configSiteUrl cc ++ "/" ++ flatten pRegStatus
                htmlQRCode sLink

                H.div ! A.class_ "person-alias-name"
                 $ H.table
                 $ do   tr $ td $ H.string "The student should scan this code"
                        tr $ td $ H.string "and be directed to the page to"
                        tr $ td $ H.string "register their own device."

                H.br
                H.div ! A.class_ "person-alias-name"
                 $ H.table
                 $ do   tr $ td $ H.string "The direct link is:"
                        tr $ td $ (H.a ! A.href (H.toValue sLink)) (H.string sLink)

cgiPersonDevLink _ inputs
 = throw $ FailNodeArgs "person device link" inputs

