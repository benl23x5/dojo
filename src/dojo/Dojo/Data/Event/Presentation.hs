
module Dojo.Data.Event.Presentation
        ( eventDisplayName
        , niceNameOfEventField)
where
import Dojo.Data.Event.Base
import Dojo.Framework


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

