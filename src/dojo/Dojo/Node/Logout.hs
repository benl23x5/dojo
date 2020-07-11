
module Dojo.Node.Logout
        (cgiLogout)
where
import Dojo.Paths
import Dojo.Framework
import qualified Network.CGI            as CGI


-- Logout of the current session.
cgiLogout :: [(String, String)] -> CGI CGIResult
cgiLogout _inputs
        = CGI.redirect $ flatten $ pathLogin

