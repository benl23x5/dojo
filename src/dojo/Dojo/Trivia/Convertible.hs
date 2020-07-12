
module Dojo.Trivia.Convertible where
import Dojo.Trivia.Base
import Dojo.Framework
import qualified Data.Time                      as Time
import Database.HDBC.SqlValue

-- toSql ----------------------------------------------------------------------
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


-- fromSql --------------------------------------------------------------------
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

