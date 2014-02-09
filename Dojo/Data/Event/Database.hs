
module Dojo.Data.Event.Database
        ( eventOfSqlValues
        , getEventList
        , getEvent
        , getEventOfLocalTime
        , insertEvent
        , updateEvent
        , getAttendance
        , getAttendanceOfPersonId
        , insertAttendance
        , deleteAttendance)
where
import Dojo.Data.Event.Base
import Dojo.Data.Person
import Dojo.Base
import qualified Data.Time      as Time


-- toSql ----------------------------------------------------------------------
instance Convertible EventId   SqlValue where
 safeConvert (EventId n)                = safeConvert n

instance Convertible EventType SqlValue where
 safeConvert (EventType str)            = safeConvert str

instance Convertible EventLocation SqlValue where
 safeConvert (EventLocation str)        = safeConvert str

instance Convertible EventDate SqlValue where
 safeConvert (EventDate ddate)          = safeConvert ddate

instance Convertible EventTime SqlValue where
 safeConvert (EventTime etime)          = safeConvert etime

instance Convertible EventLocalTime SqlValue where
 safeConvert (EventLocalTime etime)     = safeConvert etime


-- fromSql --------------------------------------------------------------------
instance Convertible SqlValue EventId where
 safeConvert val                        = liftM EventId (safeConvert val)

instance Convertible SqlValue EventType where
 safeConvert val                        = liftM EventType (safeConvert val)

instance Convertible SqlValue EventLocation where
 safeConvert val                        = liftM EventLocation (safeConvert val)

instance Convertible SqlValue EventDate where
 safeConvert val                        = liftM EventDate (safeConvert val)

instance Convertible SqlValue EventTime where
 safeConvert val                        = liftM EventTime (safeConvert val)


-------------------------------------------------------------------------------
-- | Build an event from a list of Sql values.
eventOfSqlValues :: [SqlValue] -> Event
eventOfSqlValues [eid, etype, loc, ltime]
        = Event
        { eventId       = fromSql eid
        , eventType     = fromSql etype
        , eventLocation = fromSql loc
        , eventDate     = edate
        , eventTime     = etime }

        where (edate, etime) = splitEventLocalTime $ fromSql ltime

eventOfSqlValues _ = error "eventOfValues: no match"


-------------------------------------------------------------------------------
-- | Get all the events, ordered by decending time.
getEventList :: IConnection conn => conn -> IO [(Event, Int)]
getEventList conn
 = do   valuess <- quickQuery' conn (unlines
                [ "SELECT Event.EventId, Event.Type, Event.Location, Event.Time"
                , "     , (SELECT count(PersonId) from Attendance"
                , "        WHERE  Attendance.EventId = Event.EventId)"
                , "FROM   Event"
                , "ORDER BY Time DESC" ]) []

        let elemOfSqlValues values
                | [eid, etype, loc, ttime, people] <- values
                = ( eventOfSqlValues [eid, etype, loc, ttime]
                  , fromSql people)

                | otherwise
                = error "getEventList: no match"

        return $ map elemOfSqlValues valuess


-- | Get the event with the given id.
getEvent :: IConnection conn => conn -> EventId -> IO Event
getEvent conn eid
 = do   [values] <- quickQuery' conn (unlines
                [ "SELECT EventId,Type,Location,Time"
                , "FROM  Event"
                , "WHERE EventId=?" ]) 
                [toSql eid]

        return $ eventOfSqlValues values


-- | Get the event with the given time.
getEventOfLocalTime 
        :: IConnection conn 
        => conn -> Time.LocalTime -> IO Event

getEventOfLocalTime conn ttime 
 = do   [values] <- quickQuery' conn (unlines
                [ "SELECT EventId,Type,Location,Time"
                , "FROM   Event"
                , "WHERE  Time=?"])
                [toSql ttime]

        return $ eventOfSqlValues values


-- | Get the people that attended the event with the given id.
getAttendance :: IConnection conn => conn -> EventId -> IO [Person]
getAttendance conn eid
 = do   valuess <- quickQuery' conn (unlines
                [ "SELECT " ++ intercalate "," (map ("Person." ++) personFieldNames)
                , "FROM  Person,Attendance"
                , "WHERE Person.PersonId = Attendance.PersonId"
                , "  AND EventId=?" 
                , "ORDER BY FamilyName"])
                [toSql eid]

        return  $ map personOfSqlValues valuess


-- | Get the events a person attended.
getAttendanceOfPersonId :: IConnection conn => conn -> PersonId -> IO [Event]
getAttendanceOfPersonId conn pid
 = do   valuess <- quickQuery' conn (unlines
                [ "SELECT Event.EventId, Type, Location, Time"
                , "FROM   Event,Attendance"
                , "WHERE  Event.EventId = Attendance.EventId"
                , "AND    PersonId=?"
                , "ORDER BY Time Desc"])
                [toSql pid]

        return  $ map eventOfSqlValues valuess


-- | Insert a record showing a person attended this event.
insertAttendance :: IConnection conn => conn -> EventId -> Person -> IO Integer
insertAttendance conn eid person
 = do   stmt    <- prepare conn $ unlines
                [ "INSERT INTO Attendance"
                , "(EventId, PersonId)"
                , "VALUES (?,?)" ]

        execute stmt
                [ toSql eid
                , toSql (personId person) ]


-- | Delete an attendance record.
deleteAttendance :: IConnection conn => conn -> EventId -> PersonId -> IO Integer
deleteAttendance conn eid pid
 = do   stmt    <- prepare conn $ unlines
                [ "DELETE FROM Attendance"
                , "WHERE EventId=? AND PersonId=?" ]

        execute stmt
                [ toSql eid
                , toSql pid ]


-- | Insert an event.
insertEvent :: IConnection conn => conn -> Event -> IO Integer
insertEvent conn event
 = do   stmt    <- prepare conn $ unlines
                [  "INSERT INTO Event"
                ,  "(Type,Location,Time)"
                ,  "VALUES (?,?,?)" ]

        execute stmt
                [ toSql (eventType      event)
                , toSql (eventLocation  event)
                , toSql (eventLocalTime event) ]


-- | Update an event.
updateEvent :: IConnection conn => conn -> Event -> IO Integer
updateEvent conn event
 = do   stmt    <- prepare conn $ unlines
                [ "UPDATE Event"
                , "SET Type=?,Location=?,Time=?"
                , "WHERE EventId=?" ]

        execute stmt
                [ toSql (eventType      event)
                , toSql (eventLocation  event)
                , toSql (eventLocalTime event) 
                , toSql (eventId        event) ]

