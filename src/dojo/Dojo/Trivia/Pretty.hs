
module Dojo.Trivia.Pretty where
import Dojo.Trivia.Base
import Dojo.Framework


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


