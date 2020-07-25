
module Dojo.Data.Event
        ( module Dojo.Data.Event.Base
        , module Dojo.Data.Event.Database
        , module Dojo.Data.Event.Presentation
        , module Dojo.Framework
        , diffEvent
        , loadEvent)
where
import Dojo.Data.Event.Base
import Dojo.Data.Event.Database
import Dojo.Data.Event.Presentation
import Dojo.Framework


-- Diff -----------------------------------------------------------------------
-- | Get the list of field names that are different in these two events.
diffEvent :: Event -> Event -> [String]
diffEvent e1 e2
 = concat
        [ comp "EventId"        eventId
        , comp "Type"           eventType
        , comp "Location"       eventLocation
        , comp "Date"           eventDate
        , comp "Time"           eventTime ]

 where  comp str f
         = if f e1 == f e2 then [] else [str]


-- Loading --------------------------------------------------------------------
-- | Build an event from some key, value pairs.
--   If there is something wrong with the fields then return a list of
--    field names and associated errors.
loadEvent
        :: [(String, String)]
        -> Event
        -> Either [(String, String, ParseError)] Event

loadEvent inputs event
 = let load name def
         = case lookup name inputs of
                Nothing  -> Right def
                Just str -> case parse str of
                                Right val       -> Right (Just val)
                                Left err        -> Left [(name, str, err)]
   in do
        eloc    <- load "Location" (eventLocation  event)
        etype   <- load "Type"     (eventType      event)
        edate   <- load "Date"     (eventDate      event)
        etime   <- load "Time"     (eventTime      event)

        return  $ event
                { eventType     = etype
                , eventLocation = eloc
                , eventDate     = edate
                , eventTime     = etime }


