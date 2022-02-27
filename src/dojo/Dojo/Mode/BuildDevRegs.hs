
module Dojo.Mode.BuildDevRegs where
import Dojo.Config
import Dojo.Data
import Dojo.Paths
import Dojo.Framework
import Dojo.Framework.QRCode
import Dojo.Node.PersonDevLink


-- | Build device registration forms for all people in the database.
--   The constructed forms end up in the deployment directory under data/www/g
modeBuildDevRegs :: Config -> IO ()
modeBuildDevRegs cc
 = do   conn    <- liftIO $ connectSqlite3 $ configPathDatabase cc
        ps      <- liftIO $ getPeople conn
        mapM_ (buildDevReg cc conn) ps


-- | Build a device registration form for a single person.
buildDevReg :: IConnection conn => Config -> conn -> Person -> IO ()
buildDevReg config conn person
 = do   let Just pid    = personId person
        let sSalt       = configQrSaltActive config
        sCode           <- liftIO $ acquirePersonDeviceRegCode conn sSalt pid

        let pRegStatus  = pathPersonDevStatus config sCode
        let sLink       = configSiteUrl config ++ "/" ++ flatten pRegStatus

        let sCodePng    = makeQRCode sLink
        sRegDownload    <- liftIO $ buildPersonRegPDF config pid person sCode sCodePng

        print sRegDownload
