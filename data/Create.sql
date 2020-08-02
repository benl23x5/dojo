
/* Long names of days, starting with 0 = Sunday
 * as per the Sqlite strftime function. */
CREATE TABLE v1_DimDay
        ( DayId                 INTEGER   PRIMARY KEY
        , DayName               TEXT      NOT NULL
        , UNIQUE(DayName));

CREATE TABLE v1_User
        ( UserId                INTEGER   PRIMARY KEY
        , UserName              TEXT      NOT NULL
        , UserPersonId          INTEGER   NOT NULL
        , PasswordHash          TEXT      NOT NULL
        , PasswordSalt          TEXT      NOT NULL
        , RoleNative            TEXT      NOT NULL
        , UNIQUE(UserName));

CREATE TABLE v1_Session
        ( SessionId             INTEGER   PRIMARY KEY
        , UserId                INTEGER   NOT NULL
        , Hash                  TEXT      NOT NULL
        , RoleNative            TEXT      NOT NULL
        , RoleActive            TEXT      NOT NULL
        , StartTime             DATETIME  NOT NULL
        , EndTime               DATETIME
        , UNIQUE(UserId, Hash));

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

CREATE TABLE v1_PersonMembershipLevel
        ( SortOrder             INTEGER
        , Name                  STRING  PRIMARY KEY);

CREATE TABLE v1_Event
        ( EventId               INTEGER PRIMARY KEY
        , CreatedBy             UserId  NOT NULL
        , Type                  STRING
        , Location              STRING
        , Time                  DATETIME
        , FOREIGN KEY(CreatedBy) REFERENCES v1_User(UserId));

CREATE TABLE v1_EventType
        ( SortOrder             Integer
        , Name                  STRING  PRIMARY KEY);

CREATE TABLE v1_Attendance
        ( PersonId              INTEGER
        , EventId               INTEGER
        , PRIMARY KEY(PersonId, EventId)
        , FOREIGN KEY(PersonId) REFERENCES v1_Person(PersonId)
        , FOREIGN KEY(EventId)  REFERENCES v1_Event(EventId));

/* A reoccuring weekly class, which can be used as a template
 * to create a new event. */
CREATE TABLE v1_Class
        ( ClassId               INTEGER PRIMARY KEY
        , OwnerUserName         STRING  NOT NULL
        , Type                  STRING  NOT NULL
        , Location              STRING  NOT NULL
        , Day                   STRING  NOT NULL
        , TimeStart             TIME    NOT NULL
        , TimeEnd               TIME
        , DateFirst             DATE    NOT NULL
        , DateFinal             DATE
        , FOREIGN KEY(OwnerUserName) REFERENCES v1_User(Name));

/* Dojo names. */
CREATE TABLE v1_Dojo
        ( Name                  STRING  PRIMARY KEY);

