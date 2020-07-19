
CREATE TABLE v1_User
        ( UserId                INTEGER   PRIMARY KEY
        , UserName              TEXT
        , PasswordHash          TEXT
        , PasswordSalt          TEXT
        , UserPersonId          INTEGER);

CREATE TABLE v1_Session
        ( SessionId             INTEGER   PRIMARY KEY
        , UserId                INTEGER   NOT NULL
        , Hash                  TEXT      NOT NULL
        , StartTime             DATETIME  NOT NULL
        , EndTime               DATETIME);

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

CREATE TABLE v1_Event
        ( EventId               INTEGER PRIMARY KEY
        , Type                  STRING
        , Location              STRING
        , Time                  DATETIME);

CREATE TABLE v1_Attendance
        ( PersonId              INTEGER
        , EventId               INTEGER
        , PRIMARY KEY (PersonId, EventId));

/* A reoccuring weekly class, which can be used as a template
 * to create a new event. */
CREATE TABLE v1_Class
        ( ClassId               INTEGER PRIMARY KEY
        , Type                  STRING  NOT NULL
        , Location              STRING  NOT NULL
        , Day                   STRING  NOT NULL
        , Time                  TIME    NOT NULL
        , DateFirst             DATE    NOT NULL
        , DateFinal             DATE);


