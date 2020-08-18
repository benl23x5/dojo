
module Dojo.Node.PersonDel (cgiPersonDel) where
import Dojo.Node.Logout
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Data.Session
import Dojo.Paths
import Dojo.Fail


-- | Delete an existing person.
cgiPersonDel :: Session -> [(String, String)] -> CGI CGIResult
cgiPersonDel ss inputs
 = goParseEid
 where
  goParseEid
   = do conn <- liftIO $ connectSqlite3 $ sessionDatabasePath ss

        -- Parse an existing event id if we were given one.
        case lookup "pid" inputs of
         Just strPid
          -> case parse strPid of
                Left err  -> throw $ FailParse "person id" strPid err
                Right eid -> goDelPerson conn eid
         Nothing -> cgiLogout ss

  goDelPerson conn pid
   = do person     <- liftIO $ getPerson conn pid
        attendance <- liftIO $ getAttendanceOfPersonId conn pid

        -- Only try to delete the person if they have no attendance
        -- records otherwise database will reject the deletion due
        -- to violation of foreign key constraint on attendance table,
        -- as it rightly should.
        if |  sessionIsAdmin ss
           ,  null attendance
           -> do liftIO $ deletePerson conn person
                 liftIO $ commit conn
                 liftIO $ disconnect conn
                 redirect $ flatten $ pathPersonList ss

           |  otherwise
           -> cgiLogout ss
