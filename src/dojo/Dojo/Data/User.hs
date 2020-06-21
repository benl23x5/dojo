
module Dojo.Data.User
        ( User                  (..)
        , UserId                (..)
        , UserName              (..)
        , UserPasswordHash      (..)
        , UserPasswordSalt      (..)

        -- * Constructors
        , zeroUser

        -- * Database
        , userOfSqlValues
        , getMaybeUser)
where
import Dojo.Data.User.Base
import Dojo.Data.User.Database
