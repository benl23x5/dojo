
module Dojo.Chrome where
import Dojo.Framework
import Dojo.Data.Session
import Data.String

import qualified Network.CGI                    as CGI

import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Text.Blaze.Html.Renderer.Utf8  as R


-------------------------------------------------------------------------------
-- | Output a page via CGI.
cgiPage :: Html -> CGI.CGI CGI.CGIResult
cgiPage htmlContent
 = CGI.outputFPS
 $ R.renderHtml
 $ H.docTypeHtml htmlContent


-- | Output a Navi page via CGI.
cgiPageNavi
        :: Session      -- ^ Current session.
        -> String       -- ^ Page name.
        -> String       -- ^ Name of path that is currently active.
        -> [Path]       -- ^ Navigation paths.
        -> Html         -- ^ Page body.
        -> CGI.CGI CGI.CGIResult

cgiPageNavi ss sActive sName paths htmlContent
 = cgiPage $ pageNavi ss sActive sName paths htmlContent


-- | Output a plain page via CGI
cgiPagePlain :: String -> Html -> CGI.CGI CGI.CGIResult
cgiPagePlain sName htmlContent
 = cgiPage $ pagePlain sName htmlContent


-------------------------------------------------------------------------------
-- | A complete page with navigation bar on the top.
pageNavi :: Session     -- ^ Current session.
         -> String      -- ^ Page name.
         -> String      -- ^ Name of path that is currently active.
         -> [Path]      -- ^ Navigation paths.
         -> Html -> Html

pageNavi _ss sActive sName paths htmlContents
 = do   pageHeader sName

        H.body
         $ do   H.div ! A.id "navbar"
                 $ H.table $ H.tr
                 $ do   mapM_ tdPathNav paths

                H.div ! A.id "container"
                 $ H.div ! A.id "content"
                 $ do   htmlContents

 where
        tdPathNav path
         = H.td ! A.id (fromString $ pathName path)
                !? (pathName path == sActive, A.class_, "selected")
         $ H.a  ! A.href (H.toValue path)
         $ H.toMarkup $ pathName path


-- | A complete page with no navigation bar.
pagePlain :: String     -- ^ Page Name
          -> Html -> Html

pagePlain sName htmlContents
 = do   pageHeader sName
        pageBody htmlContents


-------------------------------------------------------------------------------
-- | Construct page header including style definition
pageHeader :: String -> Html
pageHeader name
 = H.head
 $ do   H.title $ H.toMarkup name
        H.preEscapedToHtml $ unlines ssHeader
 where
  ssHeader =
   [ "<style type=\"text/css\">"
   , "<!-- @import url(\"dojo-style.css\"); -->"
   , "</style>"
   , "<link href=\"https://fonts.googleapis.com/icon?family=Material+Icons\" rel=\"stylesheet\">" ]



-- | Main body of the page.
pageBody :: Html -> Html
pageBody content
 = H.body
        $ H.div ! A.id "container"
        $ H.div ! A.id "content"
        $ content


-------------------------------------------------------------------------------
-- | Table with links that perform actions on the data.
tableActions :: [Path] -> Html
tableActions paths
 = H.div ! A.id "actions"
 $ H.table
 $ H.tr $ mapM_ tdPath paths


-------------------------------------------------------------------------------
-- | Table with navigation paths
tablePaths :: [Path] -> Html
tablePaths paths
 = H.div ! A.class_ "paths"
 $ H.table
 $ H.tr $ mapM_ tdPath paths

tdPath path
 = H.td
 $ H.a  ! A.href (H.toValue path)
         $ H.toMarkup $ pathName path

htmlPathLink path
 = H.a  ! A.href (H.toValue path)
        $ H.toMarkup $ pathName path


