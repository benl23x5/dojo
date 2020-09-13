
module Dojo.Node.ClassRegulars where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.User
import Dojo.Data.Event
import Dojo.Data.Class
import Dojo.Node.ClassView
import Dojo.Fail
import Dojo.Paths
import Dojo.Chrome
import Dojo.Framework
import qualified Text.Blaze.Html5               as H
import qualified Data.Time                      as Time
import qualified Text.Blaze.Html5.Attributes    as A


-- | Regular attendees at a given class.
cgiClassRegulars :: Session -> [(String, String)] -> CGI CGIResult
cgiClassRegulars ss inputs
 | Just strClassId  <- lookup "cid" inputs
 , Right cid        <- parse strClassId
 = do
        conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        classs  <- liftIO $ getClass conn cid

        -- Lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner     <- liftIO $ getMaybeUser conn uname
        pOwner          <- liftIO $ getPerson conn $ userPersonId uOwner

        -- Lookup regular attendees to this class over the last 90 days.
        zonedTime       <- liftIO $ Time.getZonedTime
        let ltNow       =  Time.zonedTimeToLocalTime zonedTime
        let ltStart
                = ltNow { Time.localDay
                        = Time.addDays (-90) (Time.localDay ltNow) }
        let (edateFirst, _) = splitEventLocalTime ltStart
        regulars        <- liftIO $ getRegularsOfClassId conn cid edateFirst

        liftIO $ disconnect conn

        let pathClass = pathClassView ss cid
        cgiPageNavi "Classes" (classDisplayName classs) (pathsJump ss)
         $ do   H.table
                 $ do   tr $ th "class"
                        trClassSummary classs uOwner pOwner
                        tr $ td $ (H.a ! A.href (H.toValue pathClass))
                                $ H.toMarkup $ pathName pathClass

                divRegularsList ss regulars

cgiClassRegulars _ inputs
 = throw $ FailNodeArgs "class regulars" inputs
