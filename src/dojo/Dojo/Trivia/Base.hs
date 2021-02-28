
module Dojo.Trivia.Base where
import qualified Data.Time      as Time
import Dojo.Framework.Value

-------------------------------------------------------------------------------
data SessionHash
        = SessionHash String

instance MakeValue SessionHash where
 value (SessionHash s) = S s

data SessionDate
        = SessionDate Time.Day
        deriving Show

data SessionTime
        = SessionTime Time.TimeOfDay
        deriving Show

data SessionLocalTime
        = SessionLocalTime Time.LocalTime
        deriving (Show, Eq)


-------------------------------------------------------------------------------
data UserId
        = UserId Integer
        deriving (Show, Eq, Ord)

data UserName
        = UserName String
        deriving (Show, Eq, Ord)

data UserPasswordHash
        = UserPasswordHash String
        deriving (Show, Eq)

data UserPasswordSalt
        = UserPasswordSalt String
        deriving (Show, Eq)

data UserRole
        = UserRole String
        deriving (Show, Eq)


-------------------------------------------------------------------------------
data PersonId
        = PersonId Integer
        deriving (Show, Eq, Ord)

data PersonMemberId
        = PersonMemberId Integer
        deriving (Show, Eq, Ord)

data PersonName
        = PersonName String
        deriving (Show, Eq, Ord)

data PersonDate
        = PersonDate Time.Day
        deriving (Show, Eq, Ord)

data PersonPhone
        = PersonPhone String
        deriving (Show, Eq, Ord)

data PersonEmail
        = PersonEmail String
        deriving (Show, Eq, Ord)

data PersonDojo
        = PersonDojo String
        deriving (Show, Eq, Ord)

data PersonMembershipLevel
        = PersonMembershipLevel String
        deriving (Show, Eq, Ord)

data PersonFeeStatus
        = PersonFeeStatus String
        deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
data EventId
        = EventId Integer
        deriving (Show, Eq, Ord)

data EventType
        = EventType String
        deriving (Show, Eq, Ord)

data EventLocation
        = EventLocation String
        deriving (Show, Eq, Ord)

data EventDate
        = EventDate Time.Day
        deriving (Show, Eq, Ord)

data EventTime
        = EventTime Time.TimeOfDay
        deriving (Show, Eq, Ord)

data EventDisplayName
        = EventDisplayName String
        deriving (Show, Eq, Ord)

data EventLocalTime
        = EventLocalTime Time.LocalTime
        deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
data ClassId
        = ClassId Integer
        deriving (Show, Eq, Ord)

data ClassDay
        = ClassDay String
        deriving (Show, Eq, Ord)
