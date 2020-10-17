
module Dojo.Node.PersonRegCode (cgiPersonRegCode) where
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


-------------------------------------------------------------------------------
-- | Show the device registration QR code for a person.
--      &pid=NAT   Id of the person to show code for.
cgiPersonRegCode
        :: Session -> [(String, String)]
        -> CGI CGIResult

cgiPersonRegCode ss inputs
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
                        tr $ td $ H.string "Student Device Registration"

                let cc = sessionConfig ss
                let pRegStatus = pathPersonRegStatus ss pid
                htmlQRCode
                 $ configSiteUrl cc ++ "/" ++ flatten pRegStatus

cgiPersonRegCode _ inputs
 = throw $ FailNodeArgs "person reg code" inputs