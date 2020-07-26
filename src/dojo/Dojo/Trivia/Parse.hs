
module Dojo.Trivia.Parse where
import Dojo.Trivia.Base
import Dojo.Framework


-- User -----------------------------------------------------------------------
instance Parse UserId where
 parse str      = liftM UserId $ parse str


-- Person ---------------------------------------------------------------------
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


-- Event ----------------------------------------------------------------------
instance Parse EventId where
 parse str      = liftM EventId $ parse str

instance Parse EventType where
 parse str
  | not $ null str      = Right (EventType str)
  | otherwise           = Left  (ParseError "Event Type must be non-empty")

instance Parse EventLocation where
 parse str
  | not $ null str      = Right (EventLocation str)
  | otherwise           = Left  (ParseError "Event Location must be non-empty")

instance Parse EventDate where
 parse str              = liftM EventDate $ parse str

instance Parse EventTime where
 parse str              = liftM EventTime $ parse str


-- Class ----------------------------------------------------------------------
instance Parse ClassId where
 parse str      = liftM ClassId $ parse str

instance Parse ClassDay where
 parse str      = Right (ClassDay str)
