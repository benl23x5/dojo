
module Dojo.Config where

data Config
        = Config
        { -- | "Your Organization Name"
          configSiteName        :: String

          -- | Base name of script, eg "dojo.cgi"
        , configCgiName         :: String

          -- | Full path to sqlite3 database.
        , configDatabasePath    :: FilePath }
        deriving Show

