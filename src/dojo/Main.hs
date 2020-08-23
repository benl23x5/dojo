
module Main where

import Dojo.Node.Main
import Dojo.Node.Login
import Dojo.Node.Logout
import Dojo.Node.ClassList
import Dojo.Node.ClassView
import Dojo.Node.ClassEvents
import Dojo.Node.ClassRegulars
import Dojo.Node.EventList
import Dojo.Node.EventView
import Dojo.Node.EventEdit
import Dojo.Node.EventDel
import Dojo.Node.PersonList
import Dojo.Node.PersonView
import Dojo.Node.PersonEdit
import Dojo.Node.PersonDel
import Dojo.Node.Register

import Dojo.Data.Session

import Dojo.Chrome
import Dojo.Paths
import Dojo.Config
import Dojo.Framework

import qualified System.Environment                     as S
import qualified Network.CGI                            as CGI
import qualified Text.Blaze.Html5                       as H
import qualified Text.Blaze.Html5.Attributes            as A
import qualified Text.Blaze.Html.Renderer.String        as S
import qualified Control.Exception                      as Control
import Control.Monad.Catch                              as C


main :: IO ()
main
 = do   args   <- S.getArgs
        config <- loadConfig args configDefault
        CGI.runCGI $ CGI.handleErrors
         $ C.catch (cgiTop config)
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
cgiTop :: Config -> CGI CGIResult
cgiTop cc
 = goInputs
 where

  -- See if we have a session key specified.
  goInputs
   = do inputs  <- CGI.getInputs
        if -- Debug
           | Just sJunk <- lookup "d" inputs
           -> outputFPS $ renderHtml
           $  H.docTypeHtml
           $  do pageHeader "Debug"
                 pageBody $ H.string sJunk

           -- Access via a class registration key.
           | Just sRegId <- lookup "r" inputs
           -> cgiRegister cc inputs sRegId

           -- Access via an existing session key.
           | Just sHash <- lookup "s" inputs
           -> goHash sHash inputs

           -- Default redirect to the login page.
           | otherwise
           -> cgiLogin cc inputs

  -- Lookup the current session details from the db.
  goHash hash inputs
   = do conn <- liftIO $ connectSqlite3 (configDatabasePath cc)
        mss  <- liftIO $ getSessionByHash cc conn (SessionHash hash)
        liftIO $ disconnect conn
        case mss of
         -- When we are given a session key but it is not active
         --  then redirect the page so we clear the key from
         --  the current path.
         Nothing -> CGI.redirect $ flatten $ pathLogin cc
         Just ss -> goSession ss inputs

  -- Dispatch to page handler based on the node id.
  goSession ss inputs
   -- Regular node for logged-in user.
   | Just sNode <- lookup "n" inputs
   = case sNode of
        "logout"-> cgiLogout        ss
        "main"  -> cgiMain          ss inputs
        "pl"    -> cgiPersonList    ss inputs
        "pv"    -> cgiPersonView    ss inputs
        "pe"    -> cgiPersonEdit    ss inputs
        "pd"    -> cgiPersonDel     ss inputs
        "el"    -> cgiEventList     ss inputs
        "ev"    -> cgiEventView     ss inputs
        "ee"    -> cgiEventEdit     ss inputs
        "ed"    -> cgiEventDel      ss inputs
        "cl"    -> cgiClassList     ss inputs
        "cv"    -> cgiClassView     ss inputs
        "ce"    -> cgiClassEvents   ss inputs
        "cr"    -> cgiClassRegulars ss inputs
        -- Unrecognized node name.
        _ -> CGI.redirect $ flatten $ pathLogout ss

   -- We have a valid session id, but we have no node identifier.
   -- Either the user is experimenting with the API or there is a bug.
   | otherwise
   = CGI.redirect $ flatten $ pathLogout ss

