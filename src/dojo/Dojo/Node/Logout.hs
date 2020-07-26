
module Dojo.Node.Logout (cgiLogout) where
import Dojo.Data.Session
import Dojo.Framework
import Dojo.Paths
import qualified Network.CGI            as CGI
import qualified Data.Time              as Time


-- | Logout of the current session.
cgiLogout :: Session -> CGI CGIResult
cgiLogout ss
 = do
        conn  <- liftIO $ connectSqlite3 $ sessionDatabasePath ss

        -- Use current date/time to end the session.
        zonedTime <- liftIO $ Time.getZonedTime
        let (endDate, endTime)
                = splitSessionLocalTime
                $ Time.zonedTimeToLocalTime zonedTime

        liftIO $ endSession conn endDate endTime ss
        liftIO $ commit conn
        liftIO $ disconnect conn

        CGI.redirect $ flatten $ pathLogin (sessionConfig ss)

