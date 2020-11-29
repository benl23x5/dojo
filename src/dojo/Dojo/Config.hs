
module Dojo.Config where
import Data.List


-- | Global site config.
data Config
        = Config
        { -- | "Your Organization Name"
          configSiteName        :: String

          -- | URL to root of site.
          --   "http://... " with no trailing '/'.
        , configSiteUrl         :: String

          -- | URL to the logo displayed on the login page.
        , configLogoUrl         :: String

          -- | Domain name to bind cookies to.
        , configCookieDomain    :: String

          -- | Base name for registration cookies.
        , configCookieBase      :: String

          -- | Base name of script, eg "dojo.cgi"
        , configCgiName         :: String

          -- | Full path to sqlite3 database.
        , configPathDatabase    :: FilePath

          -- | Full path to where the html files are hosted.
        , configPathHtml        :: FilePath

          -- | Full path to document build directory.
        , configPathBuild       :: FilePath

          -- | Full path to person registration latex template.
        , configPathLatexPerson :: FilePath

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
        , configSiteUrl         = "https://dojo.ouroborus.net"
        , configLogoUrl         = "https://dojo.ouroborus.net/logo-aka.jpg"
        , configCookieDomain    = "ouroborus.net"
        , configCookieBase      = "dojo"
        , configCgiName         = "dojo.cgi"
        , configPathDatabase    = "dojo.db"
        , configPathHtml        = "www"
        , configPathBuild       = "build"
        , configPathLatexPerson = "latex-person"
        , configQrSaltActive    = "salt-active"
        , configQrSaltLegacy    = "salt-legacy" }


-- | Name of cookie used for student device registration.
configCookieNameStudentReg :: Config -> String
configCookieNameStudentReg config
 = configCookieBase config ++ "-student"


-- | Load args into site config.
loadConfig :: [String] -> Config -> IO Config
loadConfig aa cc
 | [] <- aa
 = return cc

 | "-site-name" : sSiteName : rest  <- aa
 = loadConfig rest
 $ cc { configSiteName = sSiteName }

 | "-site-url" : sSiteUrl : rest  <- aa
 = loadConfig rest
 $ cc { configSiteUrl = dropWhileEnd (== '/') sSiteUrl }

 | "-logo-url" : sLogoUrl : rest <- aa
 = loadConfig rest
 $ cc { configLogoUrl = sLogoUrl }

 | "-cookie-domain" : sCookieDomain : rest <- aa
 = loadConfig rest
 $ cc { configCookieDomain = sCookieDomain }

 | "-cookie-base" : sCookieBase : rest <- aa
 = loadConfig rest
 $ cc { configCookieBase = sCookieBase }

 | "-cgi-name" : sCgiName : rest <- aa
 = loadConfig rest
 $ cc { configCgiName = sCgiName }

 | "-path-db" : sDbPath : rest <- aa
 = loadConfig rest
 $ cc { configPathDatabase = sDbPath }

 | "-path-html" : sPath : rest <- aa
 = loadConfig rest
 $ cc { configPathHtml = sPath }

 | "-path-build" : sPathBuild : rest <- aa
 = loadConfig rest
 $ cc { configPathBuild = sPathBuild }

 | "-path-latex-person" : sPath : rest <- aa
 = loadConfig rest
 $ cc { configPathLatexPerson = sPath }

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
 , " -site-url        STRING    URL of root of site."
 , " -logo-url        STRING    URL of logo to display on login."
 , " -cookie-domain   STRING    Domain name to bind cookies to."
 , " -cgi-name        STRING    Base name of script, eg dojo.cgi."
 , " -path-db         PATH      Full path to sqlite3 database."
 , " -path-build      PATH      Full path to workning build directory."
 , " -qr-salt-active  STRING    Salt to generate active QR codes."
 , " -qr-salt-legacy  STRING    Salt to generate legacy QR codes."
 , "" ]
