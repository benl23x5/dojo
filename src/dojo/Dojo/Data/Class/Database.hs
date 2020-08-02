
module Dojo.Data.Class.Database where
import Dojo.Data.Class.Base
import Dojo.Data.Event
import Dojo.Trivia
import Dojo.Fail


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


-- | Get the events associated with the class of the given id.
--   The events themselves do not have the class id has part of
--   the information, but we can use the Class and Event tables
--   to associate the informatino.
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

{- for debugging getEventsOfClasssId
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