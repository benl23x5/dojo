
CREATE TABLE Person
        ( PersonId              INTEGER PRIMARY KEY
        , MemberId              INTEGER
        , PreferedName          TEXT
        , FirstName             TEXT    NOT NULL
        , MiddleName            TEXT
        , FamilyName            TEXT    
        , DateOfBirth           DATE
        , Mobile                TEXT
        , Email                 TEXT);

CREATE TABLE Event
        ( EventId               INTEGER PRIMARY KEY
        , Type                  STRING
        , Location              STRING   NOT NULL
        , Time                  DATETIME NOT NULL);

CREATE TABLE Attendance
        ( PersonId              INTEGER
        , EventId               INTEGER
        , PRIMARY KEY (PersonId, EventId));

CREATE TABLE User
        ( UserId                INTEGER   PRIMARY KEY
        , UserName              TEXT
        , PasswordHash          TEXT
        , PasswordSalt          TEXT
        , UserPersonId          INTEGER);

CREATE TABLE Session
        ( SessionId             INTEGER   PRIMARY KEY
        , UserId                INTEGER   NOT NULL
        , Hash                  TEXT      NOT NULL
        , StartTime             DATETIME  NOT NULL
        , EndTime               DATETIME);
