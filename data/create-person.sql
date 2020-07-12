
CREATE TABLE v1_Person
        ( PersonId              INTEGER PRIMARY KEY
        , MemberId              INTEGER
        , PreferredName         TEXT
        , FirstName             TEXT    NOT NULL
        , FamilyName            TEXT
        , DateOfBirth           DATE
        , PhoneMobile           TEXT
        , PhoneFixed            TEXT
        , Email                 TEXT
        , DojoHome              TEXT
        , MembershipLevel       TEXT
        , MembershipRenewal     DATE
        , EmergencyName1        TEXT
        , EmergencyPhone1       TEXT
        , EmergencyName2        TEXT
        , EmergencyPhone2       TEXT);
