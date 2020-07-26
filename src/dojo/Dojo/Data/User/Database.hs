
module Dojo.Data.User.Database where
import Dojo.Data.User.Base
import Dojo.Trivia
import Dojo.Base

-------------------------------------------------------------------------------
-- | Build a user from a list of sql values.
userOfSqlValues :: [SqlValue] -> User
userOfSqlValues
 [ uid, uname, passwordHash, passwordSalt, pid, roleNative ]
 = User { userId                = fromSql uid
        , userName              = fromSql uname
        , userPasswordHash      = fromSql passwordHash
        , userPasswordSalt      = fromSql passwordSalt
        , userPersonId          = fromSql pid
        , userRoleNative        = fromSql roleNative }

userOfSqlValues _ = error "userOfSqlValues: no match"


-- | Get the user with the given user name
getMaybeUser
        :: IConnection conn
        => conn -> UserName -> IO (Maybe User)

getMaybeUser conn uname
 = do   result  <- quickQuery' conn (unlines
                [ "SELECT UserId,UserName,PasswordHash,PasswordSalt,UserPersonId,RoleNative"
                , "FROM  v1_User"
                , "WHERE UserName=?" ])
                [toSql uname]

        case result of
         [values] -> return $ Just $ userOfSqlValues values
         _        -> return Nothing


-- | Get the user with the given user id.
getUserOfId
        :: IConnection conn
        => conn -> UserId -> IO (Maybe User)

getUserOfId conn uid
 = do   result  <- quickQuery' conn (unlines
                [ "SELECT UserId,UserName,PasswordHash,PasswordSalt,UserPersonId,RoleNative"
                , "FROM  v1_User"
                , "WHERE UserId=?" ])
                [toSql uid]

        case result of
         [values] -> return $ Just $ userOfSqlValues values
         _        -> return Nothing

