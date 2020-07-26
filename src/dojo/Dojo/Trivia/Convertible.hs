
module Dojo.Trivia.Convertible where
import Dojo.Trivia.Base
import Dojo.Framework

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


instance Convertible PersonId   SqlValue where
 safeConvert (PersonId n)               = safeConvert n

instance Convertible PersonMemberId SqlValue where
 safeConvert (PersonMemberId mid)       = safeConvert mid

instance Convertible PersonName  SqlValue where
 safeConvert (PersonName name)          = safeConvert name

instance Convertible PersonDate  SqlValue where
 safeConvert (PersonDate dob)           = safeConvert dob

instance Convertible PersonPhone SqlValue where
 safeConvert (PersonPhone mobile)       = safeConvert mobile

instance Convertible PersonEmail  SqlValue where
 safeConvert (PersonEmail  email)       = safeConvert email

instance Convertible PersonDojo SqlValue where
 safeConvert (PersonDojo mobile)        = safeConvert mobile

instance Convertible PersonMembershipLevel SqlValue where
 safeConvert (PersonMembershipLevel mobile) = safeConvert mobile


instance Convertible EventId   SqlValue where
 safeConvert (EventId n)                = safeConvert n

instance Convertible EventType SqlValue where
 safeConvert (EventType str)            = safeConvert str

instance Convertible EventLocation SqlValue where
 safeConvert (EventLocation str)        = safeConvert str

instance Convertible EventDate SqlValue where
 safeConvert (EventDate ddate)          = safeConvert ddate

instance Convertible EventTime SqlValue where
 safeConvert (EventTime etime)          = safeConvert etime

instance Convertible EventLocalTime SqlValue where
 safeConvert (EventLocalTime etime)     = safeConvert etime



instance Convertible ClassId SqlValue where
 safeConvert (ClassId n)                = safeConvert n

instance Convertible ClassDay SqlValue where
 safeConvert (ClassDay n)               = safeConvert n



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


instance Convertible SqlValue PersonId where
 safeConvert val        = liftM PersonId (safeConvert val)

instance Convertible SqlValue PersonMemberId where
 safeConvert val        = liftM PersonMemberId (safeConvert val)

instance Convertible SqlValue PersonName where
 safeConvert val        = liftM PersonName (safeConvert val)

instance Convertible SqlValue PersonDate where
 safeConvert val        = liftM PersonDate (safeConvert val)

instance Convertible SqlValue PersonPhone where
 safeConvert val        = liftM PersonPhone (safeConvert val)

instance Convertible SqlValue PersonEmail where
 safeConvert val        = liftM PersonEmail (safeConvert val)

instance Convertible SqlValue PersonDojo where
 safeConvert val        = liftM PersonDojo (safeConvert val)

instance Convertible SqlValue PersonMembershipLevel where
 safeConvert val        = liftM PersonMembershipLevel (safeConvert val)


instance Convertible SqlValue EventId where
 safeConvert val        = liftM EventId (safeConvert val)

instance Convertible SqlValue EventType where
 safeConvert val        = liftM EventType (safeConvert val)

instance Convertible SqlValue EventLocation where
 safeConvert val        = liftM EventLocation (safeConvert val)

instance Convertible SqlValue EventDate where
 safeConvert val        = liftM EventDate (safeConvert val)

instance Convertible SqlValue EventTime where
 safeConvert val        = liftM EventTime (safeConvert val)


instance Convertible SqlValue ClassId where
 safeConvert val        = liftM ClassId (safeConvert val)

instance Convertible SqlValue ClassDay where
 safeConvert val        = liftM ClassDay (safeConvert val)
