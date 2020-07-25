
module Dojo.Node.Logout (cgiLogout) where
import Dojo.Paths
import Dojo.Framework
import qualified Network.CGI            as CGI


-- | Logout of the current session.
--   ISSUE #29: On logout, clear the current session key.
cgiLogout :: [(String, String)] -> CGI CGIResult
cgiLogout _inputs
        = CGI.redirect $ flatten $ pathLogin

