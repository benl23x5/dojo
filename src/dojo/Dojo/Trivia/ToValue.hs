
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


-------------------------------------------------------------------------------
instance ToValue PersonId where
 toValue (PersonId i) = I i

instance ToValue PersonMemberId where
 toValue (PersonMemberId i) = I i

instance ToValue PersonName where
 toValue (PersonName i) = S i

instance ToValue PersonDate where
 toValue (PersonDate i) = TD i

instance ToValue PersonPhone where
 toValue (PersonPhone i) = S i

instance ToValue PersonEmail where
 toValue (PersonEmail i) = S i

instance ToValue PersonDojo where
 toValue (PersonDojo i) = S i

instance ToValue PersonMembershipLevel where
 toValue (PersonMembershipLevel i) = S i

instance ToValue PersonFeeStatus where
 toValue (PersonFeeStatus i) = S i


-------------------------------------------------------------------------------
instance ToValue EventId where
 toValue (EventId i) = I i

instance ToValue EventType where
 toValue (EventType i) = S i

instance ToValue EventLocation where
 toValue (EventLocation i) = S i

instance ToValue EventDate where
 toValue (EventDate i) = TD i

instance ToValue EventTime where
 toValue (EventTime i) = TT i

instance ToValue EventDisplayName where
 toValue (EventDisplayName i) = S i

instance ToValue EventLocalTime where
 toValue (EventLocalTime i) = TL i


-------------------------------------------------------------------------------
instance ToValue ClassId where
 toValue (ClassId i) = I i

instance ToValue ClassDay where
 toValue (ClassDay d) = S d
