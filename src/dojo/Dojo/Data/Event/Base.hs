
module Dojo.Data.Event.Base
        ( Event                 (..)
        , EventId               (..)
        , EventType             (..)
        , EventLocation         (..)
        , EventDate             (..)
        , EventTime             (..)
        , EventDisplayName      (..)
        , EventLocalTime        (..)

        -- * Projections
        , eventLocalTime

        -- * Constructors
        , zeroEvent

        -- * Conversions
        , makeEventLocalTime
        , splitEventLocalTime)
where
import Dojo.Trivia
import qualified Data.Time      as Time


-- | An event that people can attend.
--
--   This is a view of the rows of the event table in the database.
--   We keep the date and time separate in this view, but they are merged
--   in the database.
--
data Event
        = Event
        { -- | Unique event id.
          eventId               :: EventId              -- PRIMARY KEY

          -- | Sort of event, eg "Regular"
        , eventType             :: EventType

          -- | Where the event was held.
        , eventLocation         :: EventLocation

          -- | Date the event was held.
        , eventDate             :: EventDate

          -- | Local time when the event was held.
        , eventTime             :: EventTime }


-- Projections ----------------------------------------------------------------
-- | Take the local time of an event.
eventLocalTime :: Event -> EventLocalTime
eventLocalTime event
        = EventLocalTime
        $ makeEventLocalTime (eventDate event) (eventTime event)


-- Constructors ---------------------------------------------------------------
-- | Create a zero event.
zeroEvent :: EventDate -> EventTime -> Event
zeroEvent edate etime
        = Event
        { eventId               = EventId       0
        , eventType             = EventType     ""
        , eventLocation         = EventLocation ""
        , eventDate             = edate
        , eventTime             = etime }


-- Conversions ----------------------------------------------------------------
-- | Make a `LocalTime` from the date and time portions.
makeEventLocalTime  :: EventDate -> EventTime -> Time.LocalTime
makeEventLocalTime (EventDate edate) (EventTime etime)
        = Time.LocalTime edate etime


-- | Split a `LocalTime` into the date and time portions.
splitEventLocalTime :: Time.LocalTime -> (EventDate, EventTime)
splitEventLocalTime (Time.LocalTime edate etime)
        = (EventDate edate, EventTime etime)

