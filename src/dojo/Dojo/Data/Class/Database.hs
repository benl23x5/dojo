
module Dojo.Data.Class.Database where
import Dojo.Data.Class.Base
import Dojo.Data.Person
import Dojo.Data.Event
import Dojo.Fail
import Dojo.Framework
import qualified Data.Time      as Time


------------------------------------------------------------------------------
-- | Get all the classes.
getClasses  :: IConnection conn => conn -> IO [Class]
getClasses conn
 = do   vss <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity classEntity
                , "ORDER BY Location, Day ASC"]) []

        return $ map classOfSqlValues vss


-- | Get the class with the given id.
getClass  :: IConnection conn => conn -> ClassId -> IO Class
getClass conn cid
 = do   vss     <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity classEntity
                , "WHERE ClassId=?" ])
                [ toSql cid ]

        case vss of
         [vs] -> return $ classOfSqlValues vs
         _    -> throw $ FailUnknownEntity "class" (pretty cid)


-------------------------------------------------------------------------------
-- | Get the events associated with the class of the given id.
--   The events themselves do not have the class id has part of
--   the information, but we can use the Class and Event tables
--   to associate the information.
getEventsOfClassId
        :: IConnection conn
        => conn -> ClassId -> IO [(Event, Int)]

getEventsOfClassId conn cid
 = do   valuess <- quickQuery' conn (unlines
                [ "SELECT v1_Event.EventId,"
                , "       v1_Event.Type, v1_Event.Location, v1_Event.Time,"
                , "       v1_Event.CreatedBy,"
                , "       (SELECT count(v1_Attendance.PersonId) from v1_Attendance"
                , "        WHERE  v1_Attendance.EventId = v1_Event.EventId)"
                , "FROM   v1_Event, v1_Class, v1_DimDay"
                , "WHERE  v1_Class.ClassId    = ?"
                , "AND    v1_Class.Type       = v1_Event.Type"
                , "AND    v1_Class.Location   = v1_Event.Location"
                , "AND    v1_Class.TimeStart  = time(v1_Event.Time)"
                , "AND    v1_DimDay.DayId     = strftime('%w', v1_Event.Time)"
                , "AND    v1_DimDay.DayName   = v1_Class.Day"
                , "ORDER BY v1_Event.Time DESC"])
                [toSql cid]

        let elemOfSqlValues values
                | [eid, etype, loc, ttime, createdBy, nPeople] <- values
                = ( eventOfSqlValues [eid, etype, loc, ttime, createdBy]
                  , fromSql nPeople)

                | otherwise
                = error "getEventList: no match"

        return $ map elemOfSqlValues valuess

{- Raw query for getEventsOfClasssId
SELECT ClassId, EventId,
       v1_Class.Type, v1_Class.Location,
       v1_Class.Day,  time(v1_Event.Time),
       date(v1_Event.Time)
FROM   v1_Event, v1_Class, v1_DimDay
WHERE  v1_Class.Type       = v1_Event.Type
AND    v1_Class.Location   = v1_Event.Location
AND    v1_Class.TimeStart  = time(v1_Event.Time)
AND    v1_DimDay.DayId     = strftime("%w", v1_Event.Time)
AND    v1_DimDay.DayName   = v1_Class.Day
ORDER BY v1_Event.Time DESC
-}


------------------------------------------------------------------------------
-- | Given a single event, see if there is a matching class to create
--   events at the same location and time.
getClassIdOfEventId :: IConnection conn => conn -> EventId -> IO (Maybe ClassId)
getClassIdOfEventId conn eid
 = do   valuess <- quickQuery' conn (unlines
                [ "SELECT v1_Class.ClassId"
                , "FROM   v1_Class, v1_Event, v1_DimDay"
                , "WHERE  v1_Event.EventId   = ?"
                , " AND   v1_Class.Type      = v1_Event.Type"
                , " AND   v1_Class.Location  = v1_Event.Location"
                , " AND   v1_Class.TimeStart = time(v1_Event.Time)"
                , " AND   v1_DimDay.DayId    = strftime('%w', v1_Event.Time)"
                , " AND   v1_DimDay.DayName  = v1_Class.Day"])
                [toSql eid]

        case valuess of
         [[v]]  -> return $ Just (fromSql v)
         _      -> return Nothing


-------------------------------------------------------------------------------
-- | Get the regular attendees of a class.
--   We collect the people that have attended events that match
--   the class template after the given date,
--   and sort them by number of events attended and the date of the last one.
getRegularsOfClassId
        :: IConnection conn
        => conn -> ClassId -> EventDate
        -> IO [(Person, Integer, EventDate)]

getRegularsOfClassId conn cid dateAfter
 = do
        valuess <- quickQuery' conn (unlines
                [ "SELECT "
                , intercalate "," (map ("v1_Person." ++ ) personFieldNames) ++ ","
                , "       COUNT(v1_Event.EventId) as nCount,"
                , "       MAX(date(v1_Event.Time))"
                , "FROM    v1_Event"
                , "INNER JOIN v1_Class"
                , " ON     v1_Class.Location       = v1_Event.Location"
                , " AND    v1_Class.Type           = v1_Event.Type"
                , " AND    v1_Class.TimeStart      = time(v1_Event.Time)"
                , " AND    v1_Class.ClassId        = ?"
                , " AND    date(v1_Event.Time)     > ?"
                , "INNER JOIN v1_DimDay"
                , " ON     v1_DimDay.DayId         = strftime('%w', v1_Event.Time)"
                , " AND    v1_DimDay.DayName       = v1_Class.Day"
                , "INNER JOIN v1_Attendance"
                , " ON     v1_Attendance.EventId   = v1_Event.EventId"
                , "INNER JOIN v1_Person"
                , " ON     v1_Attendance.PersonId  = v1_Person.PersonId"
                , "GROUP BY v1_Attendance.PersonId"
                , "ORDER BY nCount DESC, date(v1_Event.Time) DESC" ])
                [toSql cid, toSql dateAfter]

        let elemOfResult fs
                | vsPerson              <- take (length personFieldNames) fs
                , [vCount, vDateLast]   <- drop (length personFieldNames) fs
                = ( personOfSqlValues vsPerson
                  , fromSql vCount, fromSql vDateLast)

            elemOfResult _
                = error "elemOfResult: no match"

        return $ map elemOfResult valuess


{- Raw query for getRegularsOfClassId
SELECT  ClassId,
        COUNT(v1_Event.EventId) as nCount,
        v1_Class.Type, v1_Class.Location,
        v1_Class.Day,  time(v1_Event.Time),
        MAX(date(v1_Event.Time)),
        v1_Attendance.PersonId,
        v1_Person.FirstName,
        v1_Person.FamilyName
FROM    v1_Event
INNER JOIN v1_Class
 ON     v1_Class.Location       = v1_Event.Location
 AND    v1_Class.Type           = v1_Event.Type
 AND    v1_Class.TimeStart      = time(v1_Event.Time)
 AND    v1_Class.ClassId        = 11                            /* class id */
 AND    date(v1_Event.Time) > '2020-07-01'                      /* oldest event */
INNER JOIN v1_DimDay
 ON     v1_DimDay.DayId         = strftime('%w', v1_Event.Time)
 AND    v1_DimDay.DayName       = v1_Class.Day
INNER JOIN v1_Attendance
 ON     v1_Attendance.EventId   = v1_Event.EventId
INNER JOIN v1_Person
 ON     v1_Attendance.PersonId  = v1_Person.PersonId
GROUP BY v1_Attendance.PersonId
ORDER BY nCount DESC, date(v1_Event.Time) DESC
LIMIT   50;
-}


-------------------------------------------------------------------------------
-- | Like `getRegularsOfClassId` but use a start date from 90 days ago
--   when determining who is a regular.
getRecentRegularsOfClassId
        :: IConnection conn
        => conn -> ClassId
        -> IO [(Person, Integer, EventDate)]

getRecentRegularsOfClassId conn cid
 = do   zonedTime       <- liftIO $ Time.getZonedTime
        let ltNow       =  Time.zonedTimeToLocalTime zonedTime
        let ltStart
                = ltNow { Time.localDay
                        = Time.addDays (-90) (Time.localDay ltNow) }
        let (edateFirst, _) = splitEventLocalTime ltStart
        getRegularsOfClassId conn cid edateFirst


-------------------------------------------------------------------------------
-- | Get the list of events of the class that are on today.
--   There should only be one or none, unless there is some
--   problem with the database.
getEventsOfClassToday
        :: IConnection conn
        => conn -> ClassId
        -> IO [Event]

getEventsOfClassToday conn cid
 = do
        -- All events listed for the class.
        eventsAll <- getEventsOfClassId conn cid

        -- See if there is an event created for today already.
        zonedTime <- Time.getZonedTime
        let (edate, _etime)
                = splitEventLocalTime
                $ Time.zonedTimeToLocalTime zonedTime

        -- Events of this class that are on today.
        let eventsToday
                = [ event | (event, _pax) <- eventsAll
                  , eventDate event == Just edate ]

        return eventsToday
