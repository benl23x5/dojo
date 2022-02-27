
-- Ages of members, just based on year portion of birth date.
select FirstName, FamilyName, 2020 - strftime('%Y', DateOfBirth) as Age
from v1_Person
order by Age;

select 2020 - strftime('%Y', DateOfBirth) as Age
from v1_Person
order by Age;


-- Ages of active members, just based on the year portion of the birth date.
select 2020 - strftime('%Y', DateOfBirth) as Age
from   v1_Person as p, v1_Attendance as a, v1_Event as e
where  p.PersonId = a.PersonId
   and a.EventId  = e.EventId
group by p.PersonId, p.FirstName, p.FamilyName
order by Age;


-- Number of people by years-of-age, just based on year portion of birth date.
select Age, Count(Age)
from (select FirstName, FamilyName, 2020 - strftime('%Y', DateOfBirth) as Age
      from v1_Person order by Age)
group by Age;


select  p.PersonId, p.FirstName, p.FamilyName,
        2020 - strftime('%Y', DateOfBirth) as Age,
        count(a.EventId) as Count
from   v1_Person as p, v1_Attendance as a, v1_Event as e
where  p.PersonId = a.PersonId
   and a.EventId  = e.EventId
group by p.PersonId, p.FirstName, p.FamilyName
order by Age desc;


-- Ages of active members, just based on the year portion of the birth date.
select 2020 - strftime('%Y', DateOfBirth) as age
from   v1_Person as p, v1_Attendance as a, v1_Event as e
where  p.PersonId = a.PersonId
   and a.EventId  = e.EventId
group by p.PersonId, p.FirstName, p.FamilyName
order by age;


-- Days of training per age group.
select  2020 - strftime('%Y', DateOfBirth) as Age,
        sum(g.Count)
from    v1_Person as p,
        (select p.PersonId       as PersonId,
                p.FirstName      as FirstName,
                p.FamilyName     as FamilyName,
                count(a.EventId) as Count
        from   v1_Person as p, v1_Attendance as a, v1_Event as e
        where  p.PersonId = a.PersonId
           and a.EventId  = e.EventId
        group by p.PersonId, p.FirstName, p.FamilyName
        order by Count desc) as g
where   p.PersonId = g.PersonId
group by Age
order by Age;
