
module Dojo.Data.Event.Database where
import Dojo.Data.Event.Base
import Dojo.Data.Person
import Dojo.Base
import qualified Data.Time      as Time



-------------------------------------------------------------------------------
-- | Build an event from a list of Sql values.
eventOfSqlValues :: [SqlValue] -> Event
eventOfSqlValues [eid, etype, loc, ltime, createdBy]
 = Event
        { eventId               = fromSql eid
        , eventType             = fromSql etype
        , eventLocation         = fromSql loc
        , eventDate             = edate
        , eventTime             = etime
        , eventCreatedBy        = fromSql createdBy}

 where (edate, etime)
         = case fmap splitEventLocalTime $ fromSql ltime of
                Nothing         -> (Nothing, Nothing)
                Just (d, t)     -> (Just d, Just t)

eventOfSqlValues _ = error "eventOfValues: no match"


-------------------------------------------------------------------------------
-- | Get all the events, ordered by decending time.
getEventList :: IConnection conn => conn -> IO [(Event, Int)]
getEventList conn
 = do   valuess <- quickQuery' conn (unlines
                [ "SELECT v1_Event.EventId,"
                , "       v1_Event.Type, v1_Event.Location, v1_Event.Time,"
                , "       v1_Event.CreatedBy,"
                , "       (SELECT count(PersonId) from v1_Attendance"
                , "        WHERE  v1_Attendance.EventId = v1_Event.EventId)"
                , "FROM   v1_Event"
                , "ORDER BY Time DESC" ]) []

        let elemOfSqlValues values
                | [eid, etype, loc, ttime, createdBy, people] <- values
                = ( eventOfSqlValues [eid, etype, loc, ttime, createdBy]
                  , fromSql people)

                | otherwise
                = error "getEventList: no match"

        return $ map elemOfSqlValues valuess


-- | Get the event with the given id.
getEvent :: IConnection conn => conn -> EventId -> IO (Maybe Event)
getEvent conn eid
 = do   vss     <- quickQuery' conn (unlines
                [ "SELECT EventId, Type, Location, Time, CreatedBy"
                , "FROM  v1_Event"
                , "WHERE EventId=?" ])
                [toSql eid]

        case vss of
         []   -> return $ Nothing
         [vs] -> return $ Just $ eventOfSqlValues vs
         _    -> error "getEvent: too many results"


-- | Get the event with the given time.
getEventOfLocalTime
        :: IConnection conn
        => conn -> Time.LocalTime -> IO Event

getEventOfLocalTime conn ttime
 = do   [values] <- quickQuery' conn (unlines
                [ "SELECT EventId, Type, Location, Time, CreatedBy"
                , "FROM   v1_Event"
                , "WHERE  Time=?"])
                [toSql ttime]

        return $ eventOfSqlValues values


-- | Get the people that attended the event with the given id.
getAttendance :: IConnection conn => conn -> EventId -> IO [Person]
getAttendance conn eid
 = do   valuess <- quickQuery' conn (unlines
                [ "SELECT " ++ intercalate "," (map ("v1_Person." ++) personFieldNames)
                , "FROM  v1_Person, v1_Attendance"
                , "WHERE v1_Person.PersonId = v1_Attendance.PersonId"
                , "  AND EventId=?"
                , "ORDER BY FamilyName"])
                [toSql eid]

        return  $ map personOfSqlValues valuess


-- | Get the events a person attended.
getAttendanceOfPersonId :: IConnection conn => conn -> PersonId -> IO [Event]
getAttendanceOfPersonId conn pid
 = do   valuess <- quickQuery' conn (unlines
                [ "SELECT v1_Event.EventId, Type, Location, Time, CreatedBy"
                , "FROM   v1_Event,v1_Attendance"
                , "WHERE  v1_Event.EventId = v1_Attendance.EventId"
                , "AND    PersonId=?"
                , "ORDER BY Time Desc"])
                [toSql pid]

        return  $ map eventOfSqlValues valuess


-- | Insert a record showing a person attended this event.
insertAttendance :: IConnection conn => conn -> EventId -> Person -> IO Integer
insertAttendance conn eid person
 = do   stmt    <- prepare conn $ unlines
                [ "INSERT INTO v1_Attendance"
                , "(EventId, PersonId)"
                , "VALUES (?,?)" ]

        execute stmt
                [ toSql eid
                , toSql (personId person) ]


-- | Delete an attendance record.
deleteAttendance :: IConnection conn => conn -> EventId -> PersonId -> IO Integer
deleteAttendance conn eid pid
 = do   stmt    <- prepare conn $ unlines
                [ "DELETE FROM v1_Attendance"
                , "WHERE EventId=? AND PersonId=?" ]

        execute stmt
                [ toSql eid
                , toSql pid ]


-- | Insert an event,
--   returning a copy of the event with the new event id added.
insertEvent :: IConnection conn => conn -> Event -> IO Event
insertEvent conn_ event
 = withTransaction conn_ $ \conn
 -> do  stmt    <- prepare conn $ unlines
                [  "INSERT INTO v1_Event"
                ,  "(Type,Location,Time,CreatedBy)"
                ,  "VALUES (?,?,?,?)" ]

        execute stmt
                [ toSql (eventType      event)
                , toSql (eventLocation  event)
                , toSql (eventLocalTime event)
                , toSql (eventCreatedBy event) ]

        [[v]]   <- quickQuery' conn (unlines
                [ "SELECT last_insert_rowid()"])
                []

        let Right iEventId = safeConvert v
        return event { eventId = iEventId }


-- | Update an event.
updateEvent :: IConnection conn => conn -> Event -> IO Integer
updateEvent conn event
 = do   stmt    <- prepare conn $ unlines
                [ "UPDATE v1_Event"
                , "SET Type=?,Location=?,Time=?"
                , "WHERE EventId=?" ]

        execute stmt
                [ toSql (eventType      event)
                , toSql (eventLocation  event)
                , toSql (eventLocalTime event)
                , toSql (eventId        event) ]


-- | Delete an event.
deleteEvent :: IConnection conn => conn -> Event -> IO Integer
deleteEvent conn event
 = do   stmt    <- prepare conn $ unlines
                [ "DELETE FROM v1_Event"
                , "WHERE EventId=?" ]

        execute stmt
                [ toSql (eventId event) ]


------------------------------------------------------------------------------
-- | Get available event types.
getEventTypes :: IConnection conn => conn -> IO [EventType]
getEventTypes conn
 = do   vss <- quickQuery' conn (unlines
                [ "SELECT Name FROM v1_EventType"
                , "ORDER BY SortOrder ASC" ]) []

        let parseType [v]
             = let Just typ = fromSql v
               in  typ

        return $ map parseType vss
