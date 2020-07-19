
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
import Dojo.Data.Session
import Dojo.Paths
import Dojo.Framework
import qualified Config
import qualified Network.CGI                            as CGI
import qualified Control.Exception                      as Control
import qualified Text.Blaze.Html5                       as H
import qualified Text.Blaze.Html5.Attributes            as A
import qualified Text.Blaze.Html.Renderer.String        as S


main :: IO ()
main
 = Control.catch (CGI.runCGI (CGI.handleErrors cgiTop))
 $ (\(e :: Control.SomeException) -> sorry e)


-- | Redirect all hard errors to the issue tracker.
sorry :: Control.SomeException -> IO ()
sorry e
 = putStrLn $ S.renderHtml $ H.docTypeHtml
 $ do   H.string "Internal error in dojo server."
        H.br
        H.string "The dojo server issue tracker is at: "
        (H.a ! A.href (H.toValue sTracker))
                (H.string $ sTracker)
        H.br
        H.br
        H.string "Please include the following message in new bug reports:"
        H.br
        H.string $ show e


 where  sTracker = "http://github.com/benl23x5/dojo/issues"


-- | Top level CGI action.
cgiTop :: CGI CGIResult
cgiTop
 = do   inputs  <- CGI.getInputs

        -- Grab the current session details.
        let mHash = lookup "s" inputs
        mSession
         <- case mHash of
                Nothing
                 -> return Nothing

                Just hash
                 -> do  conn    <- liftIO $ connectSqlite3 Config.databasePath
                        ss      <- liftIO $ getSessionByHash conn (SessionHash hash)
                        liftIO $ disconnect conn
                        return $ Just ss

        -- Dispatch on the node id argument.
        let mNode = lookup "n" inputs
        case (mSession, mNode) of
         (_,       Just "login")  -> cgiLogin         inputs
         (_,       Just "logout") -> cgiLogout        inputs
         (Just ss, Just "main")   -> cgiMain       ss inputs
         (Just ss, Just "pl")     -> cgiPersonList ss inputs
         (Just ss, Just "pv")     -> cgiPersonView ss inputs
         (Just ss, Just "pe")     -> cgiPersonEdit ss inputs
         (Just ss, Just "el")     -> cgiEventList  ss inputs
         (Just ss, Just "ev")     -> cgiEventView  ss inputs
         (Just ss, Just "ee")     -> cgiEventEdit  ss inputs
         (Just ss, Just "cl")     -> cgiClassList  ss inputs

         _ -> CGI.redirect $ flatten $ pathLogin
