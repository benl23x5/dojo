
module Dojo.Data.Event.Presentation
        ( eventDisplayName
        , niceNameOfEventField)
where
import Dojo.Data.Event.Base
import Dojo.Framework
import Dojo.Base


-- Pretty ---------------------------------------------------------------------
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


-- Parse ----------------------------------------------------------------------
instance Parse EventId where
 parse str              = liftM EventId $ parse str

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


-- Presentation ---------------------------------------------------------------
-- | Take the display name of an event.
eventDisplayName :: Event -> EventDisplayName
eventDisplayName event
         = EventDisplayName
         $          (pretty $ eventLocation event)
         ++ " "  ++ (pretty $ eventType     event)
         ++ " on " ++ (pretty $ eventDate event)
         ++ " at " ++ (pretty $ eventTime event)


niceNameOfEventField :: String -> Maybe String
niceNameOfEventField str
 = case str of
        "EventId"       -> Just "id"
        "Date"          -> Just "date"
        "Time"          -> Just "time"
        "Location"      -> Just "location"
        "Type"          -> Just "type"
        _               -> Nothing

