
module Dojo.Trivia.ToValue where
import Dojo.Trivia.Base
import Dojo.Framework.Value


-------------------------------------------------------------------------------
instance ToValue SessionHash where
 toValue (SessionHash s) = S s

instance ToValue SessionDate where
 toValue (SessionDate s) = TD s

instance ToValue SessionTime where
 toValue (SessionTime t) = TT t

instance ToValue SessionLocalTime where
 toValue (SessionLocalTime t) = TL t

instance ToValue SessionId where
 toValue (SessionId t) = I t


-------------------------------------------------------------------------------
instance ToValue UserId where
 toValue (UserId i) = I i

instance ToValue UserName where
 toValue (UserName s) = S s

instance ToValue UserPasswordHash where
 toValue (UserPasswordHash s) = S s

instance ToValue UserPasswordSalt where
 toValue (UserPasswordSalt s) = S s

instance ToValue UserRole where
 toValue (UserRole s) = S s

