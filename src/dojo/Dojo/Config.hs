
module Dojo.Config where


-- | Global site config.
data Config
        = Config
        { -- | "Your Organization Name"
          configSiteName        :: String

          -- | Base name of script, eg "dojo.cgi"
        , configCgiName         :: String

          -- | Full path to sqlite3 database.
        , configDatabasePath    :: FilePath

          -- | Salt to generate the active QR codes.
        , configQrSaltActive    :: String

          -- | Salt to generate the legacy QR codes,
          --   which still work now but will be removed in the next rotation.
        , configQrSaltLegacy    :: String }
        deriving Show


-- | Default global site config.
configDefault :: Config
configDefault
        = Config
        { configSiteName        = "Aiki Dojo"
        , configCgiName         = "dojo.cgi"
        , configDatabasePath    = "dojo.db"
        , configQrSaltActive    = "salt-active"
        , configQrSaltLegacy    = "salt-legacy" }


-- | Load args into site config.
loadConfig :: [String] -> Config -> IO Config
loadConfig aa cc
 | [] <- aa
 = return cc

 | "-site-name" : sSiteName : rest  <- aa
 = loadConfig rest
 $ cc { configSiteName = sSiteName }

 | "-cgi-name" : sCgiName : rest <- aa
 = loadConfig rest
 $ cc { configCgiName = sCgiName }

 | "-db-path" : sDbPath : rest <- aa
 = loadConfig rest
 $ cc { configDatabasePath = sDbPath }

 | "-qr-salt-active" : sSalt : rest <- aa
 = loadConfig rest
 $ cc { configQrSaltActive = sSalt }

 | "-qr-salt-legacy" : sSalt : rest <- aa
 = loadConfig rest
 $ cc { configQrSaltLegacy = sSalt }

 | otherwise
 = error usage


-- | Command-line usage information.
usage :: String
usage = unlines
 [ "dojo ARGS.."
 , " -site-name       STRING    Display name of site."
 , " -cgi-name        STRING    Base name of script, eg dojo.cgi."
 , " -db-path         PATH      Full path to sqlite3 database."
 , " -qr-salt-active  STRING    Salt to generate active QR codes."
 , " -qr-salt-legacy  STRING    Salt to generate legacy QR codes."
 , "" ]