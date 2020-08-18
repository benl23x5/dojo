
module Dojo.Node.EventDel (cgiEventDel) where
import Dojo.Node.Logout
import Dojo.Data.Event
import Dojo.Data.Session
import Dojo.Paths
import Dojo.Fail


-- | Delete an existing event.
cgiEventDel :: Session -> [(String, String)] -> CGI CGIResult
cgiEventDel ss inputs
 = goParseEid
 where
  goParseEid
   = do conn <- liftIO $ connectSqlite3 $ sessionDatabasePath ss

        -- Parse an existing event id if we were given one.
        case lookup "eid" inputs of
         Just strEid
          -> case parse strEid of
                Left err  -> throw $ FailParse "event id" strEid err
                Right eid -> goGetEvent conn eid
         Nothing -> cgiLogout ss

  goGetEvent conn eid
   = do -- TODO: handle concurrent event deletion.
        Just event <- liftIO $ getEvent      conn eid
        psAttend   <- liftIO $ getAttendance conn eid

        if |  sessionOwnsEvent ss event
           ,  null psAttend
           -> do liftIO $ deleteEvent conn event
                 liftIO $ commit conn
                 liftIO $ disconnect conn
                 redirect $ flatten $ pathEventList ss

           |  otherwise
           -> cgiLogout ss




