
module Dojo.Node.Main (cgiMain) where
import Dojo.Chrome
import Dojo.Paths
import Dojo.Base
import qualified Text.Blaze.Html5               as H


-- | The top-level page.
cgiMain sid _inputs
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Main"
        pageBody
         $ do   H.h1 "Aiki Dojo"
                tablePaths $ pathsJump sid
