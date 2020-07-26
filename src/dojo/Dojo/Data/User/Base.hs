
module Dojo.Data.User.Base
        ( User                  (..)
        , UserId                (..)
        , UserName              (..)
        , UserPasswordHash      (..)
        , UserPasswordSalt      (..)
        , zeroUser)
where
import Dojo.Data.Person


-- | A user account.
--   We keep the `User` type separate from the `Person` type so that
--   a single real person can have multiple accounts at different privilidges.
data User
        = User
        { userId                :: UserId               -- PRIMARY KEY
        , userName              :: UserName

        -- | User password hash and salt.
        , userPasswordHash      :: UserPasswordHash
        , userPasswordSalt      :: UserPasswordSalt

        -- | Person the user is attached to, if any.
        , userPersonId          :: PersonId }


data UserId             = UserId Integer            deriving (Show, Eq, Ord)
data UserName           = UserName String           deriving (Show, Eq, Ord)
data UserPasswordHash   = UserPasswordHash String   deriving (Show, Eq)
data UserPasswordSalt   = UserPasswordSalt String   deriving (Show, Eq)


-- Constructors ---------------------------------------------------------------
zeroUser :: UserId -> UserName -> User
zeroUser uid uname
        = User
        { userId                = uid
        , userName              = uname
        , userPasswordHash      = UserPasswordHash ""
        , userPasswordSalt      = UserPasswordSalt ""
        , userPersonId          = PersonId 0 }
