
module Dojo.Node.ClassEvents where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.User
import Dojo.Data.Class
import Dojo.Node.EventList
import Dojo.Node.ClassView
import Dojo.Fail
import Dojo.Paths
import Dojo.Chrome
import Dojo.Framework
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | Events associated with a given class
cgiClassEvents
        :: Session -> [(String, String)]
        -> CGI CGIResult

cgiClassEvents ss inputs
 | Just strClassId  <- lookup "cid" inputs
 , Right cid        <- parse strClassId
 = do
        conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss

        -- lookup class details and events of this class.
        classs  <- liftIO $ getClass conn cid
        events  <- liftIO $ getEventsOfClassId conn cid

        -- lookup details of the class owner.
        let Just uname  = classOwnerUserName classs
        Just uOwner     <- liftIO $ getMaybeUser conn uname
        pOwner          <- liftIO $ getPerson conn $ userPersonId uOwner
        liftIO $ disconnect conn

        cgiPageNavi ss "Classes" (classDisplayName classs) (pathsJump ss)
         $ H.div ! A.class_ "class-events"
         $ do
                -- Only bother showing user id for class owner to admins.
                let muOwner
                        = if sessionIsAdmin ss
                                then Just uOwner
                                else Nothing

                H.table $ trClassSummary classs muOwner pOwner

                tableActions [ pathClassView ss cid ]

                H.div ! A.class_ "details"
                 $ divEventList ss events

cgiClassEvents _ inputs
 = throw $ FailNodeArgs "class events" inputs

