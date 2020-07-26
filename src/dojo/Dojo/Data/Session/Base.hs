
module Dojo.Data.Session.Base where
import Dojo.Trivia
import Dojo.Config
import qualified Data.Time      as Time


-- | A login session.
--   Includes static config info as well as dynamic data
--   that we store in the database .
data Session
        = Session
        { -- | Static site config.
          sessionConfig         :: Config

          -- | Numeric session id in the databse.
        , sessionId             :: SessionId    -- PRIMARY KEY

          -- | Session hash given to the user.
          --   We expose this hash to the user instead of the raw session
          --   key so that users can't guess the keys of other sessions
          --   that might be in progress.
        , sessionHash           :: SessionHash

          -- | The id of the user attached to this session.
        , sessionUserId         :: UserId
        , sessionRoleNative     :: UserRole
        , sessionRoleActive     :: UserRole

          -- | Time the session started.
        , sessionStartDate      :: SessionDate
        , sessionStartTime      :: SessionTime

          -- | Time the session ended.
        , sessionEndDate        :: Maybe SessionDate
        , sessionEndTime        :: Maybe SessionTime }
        deriving Show

data SessionId
        = SessionId Integer
        deriving (Eq, Ord, Show)


data SessionHash        = SessionHash String
data SessionDate        = SessionDate Time.Day              deriving Show
data SessionTime        = SessionTime Time.TimeOfDay        deriving Show
data SessionLocalTime   = SessionLocalTime Time.LocalTime   deriving (Show, Eq)

instance Show SessionHash where
 show (SessionHash hash) = hash


-- Projections ----------------------------------------------------------------
-- | Get the site name from a session.
sessionSiteName :: Session -> String
sessionSiteName ss
 = configCgiName $ sessionConfig ss


-- | Get the base cgi name from a session, eg "dojo.cgi"
sessionCgiName :: Session -> String
sessionCgiName ss
 = configCgiName $ sessionConfig ss


-- | Get the full database path from a session.
sessionDatabasePath :: Session -> FilePath
sessionDatabasePath ss
 = configDatabasePath $ sessionConfig ss


-- | Take the local start time time of an event.
sessionStartLocalTime :: Session -> SessionLocalTime
sessionStartLocalTime session
 = SessionLocalTime
 $ makeSessionLocalTime
        (sessionStartDate session)
        (sessionStartTime session)


-- | Take the local end time of an event.
sessionEndLocalTime :: Session -> Maybe SessionLocalTime
sessionEndLocalTime session
 | Just edate   <- sessionEndDate session
 , Just etime   <- sessionEndTime session
 = Just $ SessionLocalTime
        $ makeSessionLocalTime edate etime

 | otherwise = Nothing


-- | Check if the active role of this session is at least admin.
sessionIsAdmin :: Session -> Bool
sessionIsAdmin ss
 = sessionRoleActive ss == UserRole "Admin"


-- Conversions ----------------------------------------------------------------
-- | Make a `LocalTime` from the date and time portions.
makeSessionLocalTime  :: SessionDate -> SessionTime -> Time.LocalTime
makeSessionLocalTime (SessionDate edate) (SessionTime etime)
        = Time.LocalTime edate etime


-- | Split a `LocalTime` into the date and time portions.
splitSessionLocalTime :: Time.LocalTime -> (SessionDate, SessionTime)
splitSessionLocalTime (Time.LocalTime edate etime)
        = (SessionDate edate, SessionTime etime)

