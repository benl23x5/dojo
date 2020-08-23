
module Dojo.Node.ClassEvents where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.User
import Dojo.Data.Event
import Dojo.Data.Class
import Dojo.Node.EventList
import Dojo.Node.ClassView
import Dojo.Fail
import Dojo.Paths
import Dojo.Chrome
import qualified Text.Blaze.Html5               as H


-------------------------------------------------------------------------------
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
        classs      <- liftIO $ getClass conn cid
        events      <- liftIO $ getEventsOfClassId conn cid

        -- lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner <- liftIO $ getMaybeUser conn uname
        pOwner      <- liftIO $ getPerson conn $ userPersonId uOwner

        liftIO $ disconnect conn

        cgiClassEvents_page ss classs uOwner pOwner events

 | otherwise
 = throw $ FailNodeArgs "class events" inputs

cgiClassEvents_page ss classs uOwner pOwner events
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader $ classDisplayName classs
        pageBody
         $ do   tablePaths $ pathsJump ss

                H.table
                 $ do   tr $ th "class"
                        trClassSummary classs uOwner pOwner

                divEventList ss events


