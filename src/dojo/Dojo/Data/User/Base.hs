
module Dojo.Data.User.Base where
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
        , userPersonId          :: PersonId

        -- | The native role of this user.
        --    The active role in the sesson may be lower for testing purposes.
        , userRoleNative        :: UserRole }

