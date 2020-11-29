
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
        , UNIQUE(UserName)
        , FOREIGN KEY(UserPersonId) REFERENCES v1_Person(PersonId));

CREATE TABLE v1_Session
        ( SessionId             INTEGER   PRIMARY KEY
        , UserId                INTEGER   NOT NULL
        , Hash                  TEXT      NOT NULL
        , RoleNative            TEXT      NOT NULL
        , RoleActive            TEXT      NOT NULL
        , StartTime             DATETIME  NOT NULL
        , EndTime               DATETIME
        , UNIQUE(UserId, Hash));


/* A person known to the organization. */
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


/* Person device registration code can be used by a person to register
 * their device with the system. Registering sets a cookie in their
 * device that they can then use to register attendance to classes.
 *
 * The code is used to generate both the registration page that the
 * person sees, as well as the contents of the cookie in their device.
 *
 * If a registration code is being misused then the database admin
 * can set the 'active' field to '0' / false to disable it.
 *
 * The next time the person dev link page is visited on the site
 * a new code will be generated.
 */
CREATE TABLE v1_PersonDeviceRegCode
        ( Id                    INTEGER  PRIMARY KEY
        , TimeCreated           DATETIME NOT NULL
        , PersonId              INTEGER  NOT NULL
        , Content               STRING   NOT NULL
        , Active                INTEGER  CHECK (Active IN (0, 1))
        , FOREIGN KEY(PersonId) REFERENCES v1_Person(PersonId));


/* Possible membership levels that a person can have with
 * the organization. */
CREATE TABLE v1_PersonMembershipLevel
        ( SortOrder             INTEGER
        , Name                  STRING  PRIMARY KEY);


/* An event that people can attend.
 * The event may either be an instance of a reoccuring class,
 * or a one-off class created separately.
 */
CREATE TABLE v1_Event
        ( EventId               INTEGER PRIMARY KEY
        , CreatedBy             INTEGER NOT NULL
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


/* A reocurring weekly class, which can be used as a template
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


/* Additional adminstrator for a reoccuring class that manages
 * the attendance records, but is not the primary class owner.
 */
CREATE TABLE v1_ClassAdmin
        ( ClassId               INTEGER NOT NULL
        , AdminUserName         STRING  NOT NULL
        , PRIMARY KEY (ClassId, AdminUserName)
        , FOREIGN KEY(ClassId)       REFERENCES v1_Class(ClassId)
        , FOREIGN KEY(AdminUserName) REFERENCES v1_User(UserName));


/* Class device registration code can be used by a person to register
 * their attendance for the class.
 *
 * The code is used to generate the QR code used to register for it.
 *
 * If a registration code is being misused then the database admin
 * can set the 'active' field to '0' / false to disable it.
 *
 * The next time the class dev link page is visited on the site
 * a new code will be generated.
 */
CREATE TABLE v1_ClassDeviceRegCode
        ( Id            INTEGER  PRIMARY KEY
        , TimeCreated   DATETIME NOT NULL
        , ClassId       INTEGER  NOT NULL
        , Content       STRING   NOT NULL
        , Active        INTEGER  CHECK (Active IN (0, 1))
        , FOREIGN KEY(ClassId) REFERENCES v1_Class(ClassId));


/* Dojo names. */
CREATE TABLE v1_Dojo
        ( Name                  STRING  PRIMARY KEY);

