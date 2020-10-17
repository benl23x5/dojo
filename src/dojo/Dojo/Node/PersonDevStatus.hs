
module Dojo.Node.PersonDevStatus (cgiPersonDevStatus) where
import Dojo.Data.Person
import Dojo.Chrome
import Dojo.Config
import Dojo.Fail
import Dojo.Framework
import Data.String
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | Show the device registration QR code for a person.
--      &pid=NAT   Id of the person to show code for.
cgiPersonDevStatus
        :: Config
        -> String
        -> CGI CGIResult

cgiPersonDevStatus cc strPersonId
 | Right pid    <- parse strPersonId
 = do   conn    <- liftIO $ connectSqlite3 $ configDatabasePath cc
        person  <- liftIO $ getPerson conn pid
        liftIO $ disconnect conn

        let sName = fromMaybe "[person]" $ personAliasName person
        cgiPagePlain "Device Status"
         $ H.div ! A.class_ "person-device-status"
         $ do
                H.img
                 ! (A.src $ fromString $ configLogoUrl cc)
                 ! (A.style "width:450px;height:450px")
                 ! (A.id    "login-logo")

                H.br
                H.div ! A.class_ "person-alias-name"
                 $ H.table
                 $ do   tr $ td $ H.string sName
                        tr $ td $ H.string "Student Device Status"

cgiPersonDevStatus _ _
 = throw $ FailNodeArgs "person device status" []

