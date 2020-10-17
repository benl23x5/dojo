
module Dojo.Node.ClassDevReg where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.User
import Dojo.Data.Class
import Dojo.Node.ClassView
import Dojo.Paths
import Dojo.Chrome
import Dojo.Fail
import Dojo.Config
import Dojo.Framework
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | Class device registration.
cgiClassDevReg :: Config -> String -> CGI CGIResult
cgiClassDevReg cc sRegId
 | strClassId  <- sRegId                        -- TODO: proper reg. code.
 , Right cid   <- parse strClassId
 = do
        conn    <- liftIO $ connectSqlite3 $ configDatabasePath cc
        classs  <- liftIO $ getClass conn cid

        -- Lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner     <- liftIO $ getMaybeUser conn uname
        pOwner          <- liftIO $ getPerson conn $ userPersonId uOwner

        cgiPagePlain (classDisplayName classs)
         $ H.div ! A.class_ "class-dev-reg"
         $ do

                H.table $ trClassSummary classs Nothing pOwner

cgiClassDevReg _ _
 = throw $ FailNodeArgs "class device registration" []

