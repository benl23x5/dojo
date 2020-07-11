
module Dojo.Data.Person.Presentation where
import Dojo.Data.Person.Base
import Dojo.Framework
import Dojo.Base


-- Pretty ---------------------------------------------------------------------
instance Pretty PersonId where
 pretty (PersonId pid)          = show pid

instance Pretty PersonMemberId where
 pretty (PersonMemberId mid)    = show mid

instance Pretty PersonName where
 pretty (PersonName name)       = name

instance Pretty PersonDate where
 pretty (PersonDate (Just day)) = pretty day
 pretty (PersonDate Nothing)    = ""

instance Pretty PersonPhone where
 pretty (PersonPhone phone)     = phone

instance Pretty PersonEmail where
 pretty (PersonEmail  email)    = email

instance Pretty PersonDojo where
 pretty (PersonDojo dojo)       = dojo

instance Pretty PersonMembershipLevel where
 pretty (PersonMembershipLevel level) = level


-- Parse ----------------------------------------------------------------------
instance Parse PersonId where
 parse str      = liftM PersonId        $ parse str

instance Parse PersonMemberId where
 parse str      = liftM PersonMemberId  $ parse str

instance Parse PersonName where
 parse str      = Right (PersonName str)

instance Parse PersonDate where
 parse str
  | str == ""   = Right (PersonDate Nothing)
  | otherwise   = liftM (PersonDate . Just) $ parse str

instance Parse PersonPhone where
 parse str      = Right (PersonPhone str)

instance Parse PersonEmail  where
 parse str      = Right (PersonEmail str)

instance Parse PersonDojo where
 parse str      = Right (PersonDojo str)

instance Parse PersonMembershipLevel where
 parse str      = Right (PersonMembershipLevel str)

