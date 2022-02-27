
/* Report number of attendees per class. */
SELECT  ClassId,
        v1_Class.Location, v1_Class.Day,
        date(v1_Event.Time) as Time,
        time(v1_Event.Time) as Date,
        COUNT(v1_Attendance.PersonId) as Attendees
FROM    v1_Event
INNER JOIN v1_Class
 ON     v1_Class.Location       = v1_Event.Location
 AND    v1_Class.Type           = v1_Event.Type
 AND    v1_Class.TimeStart      = time(v1_Event.Time)
INNER JOIN v1_DimDay
 ON     v1_DimDay.DayId         = strftime('%w', v1_Event.Time)
 AND    v1_DimDay.DayName       = v1_Class.Day
INNER JOIN v1_Attendance
 ON     v1_Attendance.EventId   = v1_Event.EventId
INNER JOIN v1_Person
 ON     v1_Attendance.PersonId  = v1_Person.PersonId
GROUP BY ClassId, v1_Event.Time
ORDER BY ClassId;


/* Report number of attendees per date. */
SELECT  date(v1_Event.Time) as Date,
        COUNT(v1_Attendance.PersonId) as Attendees
FROM    v1_Event
INNER JOIN v1_Class
 ON     v1_Class.Location       = v1_Event.Location
 AND    v1_Class.Type           = v1_Event.Type
 AND    v1_Class.TimeStart      = time(v1_Event.Time)
INNER JOIN v1_DimDay
 ON     v1_DimDay.DayId         = strftime('%w', v1_Event.Time)
 AND    v1_DimDay.DayName       = v1_Class.Day
INNER JOIN v1_Attendance
 ON     v1_Attendance.EventId   = v1_Event.EventId
INNER JOIN v1_Person
 ON     v1_Attendance.PersonId  = v1_Person.PersonId
GROUP BY Date
ORDER BY Date;

