
module Dojo.Chrome where
import Dojo.Framework
import Dojo.Base
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


pageHeader :: String -> Html
pageHeader name
 = H.head
 $ do   H.title $ H.toMarkup name
        H.preEscapedToHtml $ unlines
                [ "<style type=\"text/css\">"
                , "<!-- @import url(\"dojo-style.css\"); -->"
                , "</style>" ]

pageBody :: Html -> Html
pageBody content
        = H.body 
        $ H.div ! A.id "container"
        $ H.div ! A.id "content"
        $ content


tablePaths :: [Path] -> Html
tablePaths paths
 = H.table
 $ H.tr $ mapM_ tdPath paths

 where  tdPath path
         = H.td
         $ H.a  ! A.href (H.toValue path) 
                $ H.toMarkup $ pathName path
