
module Dojo.Data.Event.Base where
import Dojo.Framework
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
        , eventTime             :: Maybe EventTime

          -- | The user that created this event.
          --   TODO: change created by to 'owner', as the event
          --   may be created by the system due to hit on the reg. page.
        , eventCreatedBy        :: Maybe UserId }
        deriving Show


-- | Create a zero event.
zeroEvent :: Event
zeroEvent
        = Event
        { eventId               = Nothing
        , eventType             = Nothing
        , eventLocation         = Nothing
        , eventDate             = Nothing
        , eventTime             = Nothing
        , eventCreatedBy        = Nothing }


-- Entity ---------------------------------------------------------------------
-- | Event entity.
eventEntity :: Entity Event
eventEntity
        = Entity
        { entityTable   = "v1_Event"
        , entityKey     = "EvendId"
        , entityFields  = eventFields }


-- | Field definitions of the event entity.
eventFields :: [Field Event]
eventFields
 = [ Field "EventId"    "id"
        (\s -> fmap toSql $ (parse s :: Either ParseError EventId))
        (toSql . eventId)
        (\v x -> x { eventId = fromSql v})

   , Field "Type"       "type"
        (fmap toSql . loadInput @EventType)
        (toSql . eventType)
        (\v x -> x { eventType = fromSql v})

   , Field "Location"    "location"
        (fmap toSql . loadInput @EventLocation)
        (toSql . eventLocation)
        (\v x -> x { eventLocation = fromSql v})

   , Field "Date"       "date"
        (fmap toSql . loadInput @EventDate)
        (toSql . eventDate)
        (\v x -> x { eventDate = fromSql v})

   , Field "Time"       "time"
        (fmap toSql . loadInput @EventTime)
        (toSql . eventTime)
        (\v x -> x { eventTime = fromSql v})

   , Field "CreatedBy"  "created by"
        (fmap toSql . loadInput @UserId)
        (toSql . eventCreatedBy)
        (\v x -> x { eventCreatedBy = fromSql v})
   ]


-- Projections ----------------------------------------------------------------
-- | Take the local time of an event.
eventLocalTime :: Event -> Maybe EventLocalTime
eventLocalTime event
 | Just edate <- eventDate event
 , Just etime <- eventTime event
 = Just $ EventLocalTime $ makeEventLocalTime edate etime

 | otherwise
 = Nothing


-- | Pretty print an event type to use when describing a specific class.
--   We want to use simple words like "Children" in the database,
--   but when displaying info to the user say "Children's class".
--   It's not worth the effort making this configurable in the database.
eventTypeClassName :: EventType -> String
eventTypeClassName (EventType sType)
 = case sType of
        "General"       -> "General class"
        "Beginner"      -> "Beginner's class"
        "Children"      -> "Children's class"
        "Online"        -> "Online class"
        "TTC"           -> "TTC class"
        "Winter School" -> "Winter school class"
        "Summer School" -> "Summer school class"
        "Misogi"        -> "Misogi"
        "Special"       -> "Special class"
        etype           -> etype ++ " class"


-- Constructors ---------------------------------------------------------------
-- | Load differences to an event record specified in a query path.
loadEvent
        :: [(String, String)]   -- ^ Table field name and new value.
        -> Event                -- ^ Old event to be updated.
        -> Either [LoadError] Event
loadEvent = loadEntity eventEntity


-- Conversions ----------------------------------------------------------------
-- | Make a `LocalTime` from the date and time portions.
makeEventLocalTime  :: EventDate -> EventTime -> Time.LocalTime
makeEventLocalTime (EventDate edate) (EventTime etime)
        = Time.LocalTime edate etime


-- | Split a `LocalTime` into the date and time portions.
splitEventLocalTime :: Time.LocalTime -> (EventDate, EventTime)
splitEventLocalTime (Time.LocalTime edate etime)
        = (EventDate edate, EventTime etime)


-- Comparisons  ---------------------------------------------------------------
-- | Get the table field names of fields that differ in two event records.
diffEvent :: Event -> Event -> [String]
diffEvent = diffEntity eventEntity
