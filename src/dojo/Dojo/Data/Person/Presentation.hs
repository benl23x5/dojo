
module Dojo.Data.Person.Presentation
        ( niceNameOfPersonField)
where
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

instance Pretty PersonDateOfBirth where
 pretty (PersonDateOfBirth (Just day)) = pretty day
 pretty (PersonDateOfBirth Nothing)    = ""

instance Pretty PersonMobile where
 pretty (PersonMobile mobile)   = mobile

instance Pretty PersonEmail where
 pretty (PersonEmail  email)    = email


-- Parse ----------------------------------------------------------------------
instance Parse PersonId where
 parse str      = liftM PersonId        $ parse str

instance Parse PersonMemberId where
 parse str      = liftM PersonMemberId  $ parse str

instance Parse PersonName where
 parse str      = Right (PersonName str)

instance Parse PersonDateOfBirth where
 parse str
  | str == ""   = Right (PersonDateOfBirth Nothing)
  | otherwise   = liftM (PersonDateOfBirth . Just) $ parse str

instance Parse PersonMobile where
 parse str      = Right (PersonMobile str)

instance Parse PersonEmail  where
 parse str      = Right (PersonEmail str)


-- | Nice name for field labels
niceNameOfPersonField :: String -> Maybe String
niceNameOfPersonField str
 = case str of
        "PersonId"      -> Just "id"
        "MemberId"      -> Just "member id"
        "PreferredName" -> Just "preferred name"
        "FirstName"     -> Just "first name"
        "FamilyName"    -> Just "family name"
        "MiddleName"    -> Just "middle name"
        "DateOfBirth"   -> Just "date of birth"
        "Mobile"        -> Just "mobile"
        "Email"         -> Just "email"
        _               -> Nothing

