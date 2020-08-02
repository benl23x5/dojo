
module Dojo.Node.Register (cgiRegister) where
import Dojo.Framework
import Dojo.Chrome
import qualified Text.Blaze.Html5               as H


cgiRegister
 :: [(String, String)]  -- ^ Inputs
 -> String              -- ^ Registration Id.
 -> CGI CGIResult

cgiRegister _inputs sRegId
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Register"
        H.string $ "register " ++ sRegId