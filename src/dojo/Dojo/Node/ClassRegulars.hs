
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
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Data.Time                      as Time
import qualified Data.List                      as List


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

        regulars
         <- fmap (List.sortOn (\(person, _, _) -> personDisplayName person))
          $ liftIO $ getRegularsOfClassId conn cid edateFirst

        liftIO $ disconnect conn

        cgiPageNavi ss "Classes" (classDisplayName classs) (pathsJump ss)
         $ H.div ! A.class_ "class-regulars"
         $ do
                -- Only bother showing user id for class owner to admins.
                let muOwner
                        = if sessionIsAdmin ss
                                then Just uOwner
                                else Nothing

                H.table $ trClassSummary classs muOwner pOwner

                tableActions [ pathClassView ss cid ]

                H.div ! A.class_ "details"
                 $ divRegularsList ss regulars


cgiClassRegulars _ inputs
 = throw $ FailNodeArgs "class regulars" inputs
