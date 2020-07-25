
module Dojo.Data.Event.Presentation
        ( eventDisplayName
        , niceNameOfEventField)
where
import Dojo.Data.Event.Base
import Dojo.Framework
import Dojo.Trivia


-- | Take the display name of an event.
eventDisplayName :: Event -> EventDisplayName
eventDisplayName event
         = EventDisplayName
         $  (fromMaybe "event" (fmap pretty $ eventLocation event))
         ++ (case eventDate event of
                Nothing -> ""
                Just d  -> " on " ++ pretty d)
         ++ (case eventTime event of
                Nothing -> ""
                Just t  -> " at " ++ pretty t)


niceNameOfEventField :: String -> Maybe String
niceNameOfEventField str
 = case str of
        "EventId"       -> Just "id"
        "Date"          -> Just "date"
        "Time"          -> Just "time"
        "Location"      -> Just "location"
        "Type"          -> Just "type"
        _               -> Nothing

