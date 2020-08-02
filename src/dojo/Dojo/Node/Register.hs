
module Dojo.Node.Register (cgiRegister) where
import Dojo.Data.Class
import Dojo.Framework
import Dojo.Chrome
import Dojo.Config
import qualified Text.Blaze.Html5               as H


cgiRegister
 :: Config
 -> [(String, String)]  -- ^ Inputs
 -> String              -- ^ Registration Id.
 -> CGI CGIResult

cgiRegister cc _inputs sRegId
 = do   conn    <- liftIO $ connectSqlite3 $ configDatabasePath cc
        classs  <- liftIO $ getClasses conn

        let sUrl  = configSiteUrl cc
        let sSalt = configQrSaltActive cc

        let lsActive
              =  catMaybes $ flip map classs $ \cls
              -> case registrationLinkOfClass sUrl sSalt cls of
                  Just (_sUrl, sId) -> Just (sId, cls)
                  Nothing           -> Nothing

        case lookup sRegId lsActive of
         Just cls -> cgiRegister_class conn sRegId cls
         Nothing  -> cgiRegister_unknown sRegId

cgiRegister_class conn sRegId cls
 = do
        let Just cid = classId cls
        events <- liftIO $ getEventsOfClassId conn cid

        outputFPS $ renderHtml
         $ H.docTypeHtml
         $ do   pageHeader "Register"
                H.string $ "registration link valid " ++ sRegId ++ " " ++ show cls
                H.br
                H.string $ show events


cgiRegister_unknown sRegId
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Register"
        H.string $ "registration link unknown " ++ sRegId