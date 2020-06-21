
module Main where
import Dojo.Node.Login
import Dojo.Node.Logout
import Dojo.Node.Main
import Dojo.Node.PersonList
import Dojo.Node.PersonAdd
import Dojo.Node.PersonView
import Dojo.Node.PersonEdit
import Dojo.Node.EventList
import Dojo.Node.EventView
import Dojo.Node.EventEdit
import Dojo.Data.Session
import Dojo.Paths
import Dojo.Framework
import Dojo.Base hiding (main)
import qualified Config
import qualified Network.CGI            as CGI


main :: IO ()
main
 = CGI.runCGI (CGI.handleErrors cgiTop)


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

        -- Dispatch on the node id.
        let mNode       = lookup "n" inputs

        case (mSession, mNode) of
         (_,            Just "login")  -> cgiLogin         inputs
         (_,            Just "logout") -> cgiLogout        inputs
         (Just ss,      Just "main")   -> cgiMain       ss inputs
         (Just ss,      Just "pList")  -> cgiPersonList ss inputs
         (Just ss,      Just "pAdd")   -> cgiPersonAdd  ss inputs
         (Just ss,      Just "pView")  -> cgiPersonView ss inputs
         (Just ss,      Just "pEdit")  -> cgiPersonEdit ss inputs
         (Just ss,      Just "eList")  -> cgiEventList  ss inputs
         (Just ss,      Just "eView")  -> cgiEventView  ss inputs
         (Just ss,      Just "eEdit")  -> cgiEventEdit  ss inputs

         _              -> CGI.redirect $ flatten $ pathLogin
