
module Dojo.Data.User.Database
        ( userOfSqlValues
        , getMaybeUser)
where
import Dojo.Data.User.Base
import Dojo.Base


-- toSql ----------------------------------------------------------------------
instance Convertible UserId SqlValue where
 safeConvert (UserId uid)               = safeConvert uid

instance Convertible UserName SqlValue where
 safeConvert (UserName uname)           = safeConvert uname

instance Convertible UserPasswordHash SqlValue where
 safeConvert (UserPasswordHash str)     = safeConvert str

instance Convertible UserPasswordSalt SqlValue where
 safeConvert (UserPasswordSalt str)     = safeConvert str

instance Convertible UserRole SqlValue where
 safeConvert (UserRole str)             = safeConvert str


-- fromSql --------------------------------------------------------------------
instance Convertible SqlValue UserId where
 safeConvert val        = liftM UserId (safeConvert val)

instance Convertible SqlValue UserName where
 safeConvert val        = liftM UserName (safeConvert val)

instance Convertible SqlValue UserPasswordHash where
 safeConvert val        = liftM UserPasswordHash (safeConvert val)

instance Convertible SqlValue UserPasswordSalt where
 safeConvert val        = liftM UserPasswordSalt (safeConvert val)

instance Convertible SqlValue UserRole where
 safeConvert val        = liftM UserRole (safeConvert val)


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
getMaybeUser :: IConnection conn => conn -> UserName -> IO (Maybe User)
getMaybeUser conn uname
 = do   result  <- quickQuery' conn (unlines
                [ "SELECT UserId,UserName,PasswordHash,PasswordSalt,UserPersonId,RoleNative"
                , "FROM  v1_User"
                , "WHERE UserName=?" ])
                [toSql uname]

        case result of
         [values] -> return $ Just $ userOfSqlValues values
         _        -> return Nothing

