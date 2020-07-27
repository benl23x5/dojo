
module Dojo.Trivia.Pretty where
import Dojo.Trivia.Base
import Dojo.Framework


-- User -----------------------------------------------------------------------
instance Pretty UserName where
 pretty (UserName name)         = name


-- Person ---------------------------------------------------------------------
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


-- Event ----------------------------------------------------------------------
instance Pretty EventId where
 pretty (EventId n)             = show n

instance Pretty EventType where
 pretty (EventType str)         = str

instance Pretty EventLocation where
 pretty (EventLocation str)     = str

instance Pretty EventDate where
 pretty (EventDate edate)       = pretty edate

instance Pretty EventTime where
 pretty (EventTime etime)       = pretty etime

instance Pretty EventDisplayName where
 pretty (EventDisplayName str)  = str

instance Pretty EventLocalTime where
 pretty (EventLocalTime  ltime) = show ltime


-- Class ----------------------------------------------------------------------
instance Pretty ClassId where
 pretty (ClassId n)             = show n

instance Pretty ClassDay where
 pretty (ClassDay str)          = str
