
module Dojo.Trivia.Parse where
import Dojo.Trivia.Base
import Dojo.Framework


instance Parse PersonId where
 parse str      = liftM PersonId $ parse str

instance Parse PersonMemberId where
 parse str      = liftM PersonMemberId $ parse str

instance Parse PersonName where
 parse str      = Right (PersonName str)

instance Parse PersonDate where
 parse str
  | str == ""   = Right (PersonDate Nothing)
  | otherwise   = liftM (PersonDate . Just) $ parse str

instance Parse PersonPhone where
 parse str      = Right (PersonPhone str)

instance Parse PersonEmail where
 parse str      = Right (PersonEmail str)

instance Parse PersonDojo where
 parse str      = Right (PersonDojo str)

instance Parse PersonMembershipLevel where
 parse str      = Right (PersonMembershipLevel str)

