
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

-- Intrinsic
data EventId            = EventId       Integer                 deriving Eq
data EventType          = EventType     String                  deriving Eq
data EventLocation      = EventLocation String                  deriving Eq
data EventDate          = EventDate     Time.Day                deriving Eq
data EventTime          = EventTime     Time.TimeOfDay          deriving Eq

-- Synthetic
data EventDisplayName   = EventDisplayName String               deriving Eq
data EventLocalTime     = EventLocalTime   Time.LocalTime       deriving Eq


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

