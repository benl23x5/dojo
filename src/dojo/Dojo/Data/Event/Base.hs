
module Dojo.Data.Event.Base where
import Dojo.Trivia
import qualified Data.Time      as Time


-------------------------------------------------------------------------------
-- | An event that people can attend.
--
--   This is a view of the rows of the event table in the database.
--   We keep the date and time separate in this view, but they are merged
--   in the database.
--
data Event
        = Event
        { -- | The event id will be Nothing if the record has not been
          --   added to the database yet, but there is a PRIMARY KEY
          --   constraint on the field in the database.
          eventId               :: Maybe EventId        -- PRIMARY KEY

          -- | Sort of event, eg "Adults"
        , eventType             :: Maybe EventType

          -- | Where the event was held.
        , eventLocation         :: Maybe EventLocation

          -- | Date the event was held.
        , eventDate             :: Maybe EventDate

          -- | Local time when the event was held.
        , eventTime             :: Maybe EventTime }


-- | Create a zero event.
zeroEvent :: Event
zeroEvent
        = Event
        { eventId               = Nothing
        , eventType             = Nothing
        , eventLocation         = Nothing
        , eventDate             = Nothing
        , eventTime             = Nothing }


-- Projections ----------------------------------------------------------------
-- | Take the local time of an event.
eventLocalTime :: Event -> Maybe EventLocalTime
eventLocalTime event
 | Just date <- eventDate event
 , Just time <- eventTime event
 = Just $ EventLocalTime $ makeEventLocalTime date time

 | otherwise
 = Nothing


-- Constructors ---------------------------------------------------------------


-- Conversions ----------------------------------------------------------------
-- | Make a `LocalTime` from the date and time portions.
makeEventLocalTime  :: EventDate -> EventTime -> Time.LocalTime
makeEventLocalTime (EventDate edate) (EventTime etime)
        = Time.LocalTime edate etime


-- | Split a `LocalTime` into the date and time portions.
splitEventLocalTime :: Time.LocalTime -> (EventDate, EventTime)
splitEventLocalTime (Time.LocalTime edate etime)
        = (EventDate edate, EventTime etime)

