
module Dojo.Trivia.Base where
import qualified Data.Time      as Time


data PersonId
        = PersonId Integer
        deriving (Show, Eq, Ord)

data PersonMemberId
        = PersonMemberId Integer
        deriving (Show, Eq)

data PersonName
        = PersonName String
        deriving (Show, Eq)

data PersonDate
        = PersonDate (Maybe Time.Day)
        deriving (Show, Eq)

data PersonPhone
        = PersonPhone String
        deriving (Show, Eq)

data PersonEmail
        = PersonEmail String
        deriving (Show, Eq)

data PersonDojo
        = PersonDojo String
        deriving (Show, Eq)

data PersonMembershipLevel
        = PersonMembershipLevel String
        deriving (Show, Eq)

