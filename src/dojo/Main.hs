
module Main where

import Dojo.Node.Login
import Dojo.Node.Logout
import Dojo.Node.Main
import Dojo.Node.PersonList
import Dojo.Node.PersonView
import Dojo.Node.PersonEdit
import Dojo.Node.EventList
import Dojo.Node.EventView
import Dojo.Node.EventEdit
import Dojo.Node.ClassList
import Dojo.Node.ClassView
import Dojo.Data.Session

import Dojo.Paths
import Dojo.Framework

import qualified Config
import qualified Network.CGI                            as CGI
import qualified Control.Exception                      as Control
import qualified Text.Blaze.Html5                       as H
import qualified Text.Blaze.Html5.Attributes            as A
import qualified Text.Blaze.Html.Renderer.String        as S
import Control.Monad.Catch                              as C


main :: IO ()
main
 = CGI.runCGI $ CGI.handleErrors
 $ C.catch cgiTop
 $ (\(e :: Control.SomeException) -> sorry e)


-- | Redirect all hard errors to the issue tracker.
sorry :: Control.SomeException -> CGI CGIResult
sorry e
 = CGI.output $ S.renderHtml $ H.docTypeHtml
 $ do   H.string "Internal error in dojo server."
        H.br
        H.string "The dojo server issue tracker is at: "
        (H.a ! A.href (H.toValue sTracker))
                (H.string $ sTracker)
        H.br
        H.br
        H.string "Please include the following message in new bug reports:"
        H.br
        H.pre $ H.string $ show e


 where  sTracker = "http://github.com/benl23x5/dojo/issues"


-- | Top level CGI action.
cgiTop :: CGI CGIResult
cgiTop
 = goInputs
 where

  -- See if we have a session key specified.
  goInputs
   = do inputs  <- CGI.getInputs
        let mHash = lookup "s" inputs
        case mHash of
         Nothing   -> cgiLogin inputs
         Just hash -> goHash hash inputs

  -- Lookup the current session details from the db.
  goHash hash inputs
   = do conn <- liftIO $ connectSqlite3 Config.databasePath
        mss  <- liftIO $ getSessionByHash conn (SessionHash hash)
        liftIO $ disconnect conn
        case mss of
         -- When we are given a session key but it is not active
         --  then redirect the page so we clear the key from
         --  the current path.
         Nothing -> CGI.redirect $ flatten $ pathLogin
         Just ss -> goSession ss inputs

  -- Dispatch to page handler based on the node id.
  goSession ss inputs
   = do let mNode = lookup "n" inputs
        case mNode of
         Just "logout"  -> cgiLogout     ss
         Just "main"    -> cgiMain       ss inputs
         Just "pl"      -> cgiPersonList ss inputs
         Just "pv"      -> cgiPersonView ss inputs
         Just "pe"      -> cgiPersonEdit ss inputs
         Just "el"      -> cgiEventList  ss inputs
         Just "ev"      -> cgiEventView  ss inputs
         Just "ee"      -> cgiEventEdit  ss inputs
         Just "cl"      -> cgiClassList  ss inputs
         Just "cv"      -> cgiClassView  ss inputs

         -- If there is no node name, or it is not recognized
         --  then just logout the current session, which will
         --  redirect back to the login page.
         _  -> CGI.redirect $ flatten $ pathLogout ss

