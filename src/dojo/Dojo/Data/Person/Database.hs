
module Dojo.Data.Person.Database
        ( -- * Constructors
          personOfSqlValues

          -- * Database operators
        , getPeople
        , getPerson
        , insertPerson
        , updatePerson)
where
import Dojo.Data.Person.Base
import Dojo.Base


-- toSql ----------------------------------------------------------------------
instance Convertible PersonId   SqlValue where
 safeConvert (PersonId n)               = safeConvert n

instance Convertible PersonMemberId SqlValue where
 safeConvert (PersonMemberId mid)       = safeConvert mid

instance Convertible PersonName  SqlValue where
 safeConvert (PersonName name)          = safeConvert name

instance Convertible PersonDateOfBirth  SqlValue where
 safeConvert (PersonDateOfBirth dob)    = safeConvert dob

instance Convertible PersonMobile  SqlValue where
 safeConvert (PersonMobile mobile)      = safeConvert mobile

instance Convertible PersonEmail  SqlValue where
 safeConvert (PersonEmail  email)       = safeConvert email


-- fromSql --------------------------------------------------------------------
instance Convertible SqlValue PersonId where
 safeConvert val        = liftM PersonId (safeConvert val)

instance Convertible SqlValue PersonMemberId where
 safeConvert val        = liftM PersonMemberId (safeConvert val)

instance Convertible SqlValue PersonName where
 safeConvert val        = liftM PersonName (safeConvert val)

instance Convertible SqlValue PersonDateOfBirth where
 safeConvert val        = liftM PersonDateOfBirth (safeConvert val)

instance Convertible SqlValue PersonMobile where
 safeConvert val        = liftM PersonMobile (safeConvert val)

instance Convertible SqlValue PersonEmail where
 safeConvert val        = liftM PersonEmail (safeConvert val)


-------------------------------------------------------------------------------
-- | Build a person from a list of Sql values.
personOfSqlValues :: [SqlValue] -> Person
personOfSqlValues
        [ pid, memberId
        , preferedName, firstName, middleName, familyName, dateOfBirth
        , mobile, email ]

        = Person
        { personId              = fromSql pid
        , personMemberId        = fromSql memberId
        , personPreferredName   = fromSql preferedName
        , personFirstName       = fromSql firstName
        , personMiddleName      = fromSql middleName
        , personFamilyName      = fromSql familyName
        , personDateOfBirth     = fromSql dateOfBirth
        , personMobile          = fromSql mobile
        , personEmail           = fromSql email }

personOfSqlValues _ = error "personOfValues: no match"


-- | Get all the people, ordered by family name.
getPeople  :: IConnection conn => conn -> IO [Person]
getPeople conn
 = do   valuess <- quickQuery' conn (unlines
                [ "SELECT PersonId,MemberId"
                , ",PreferedName,FirstName,MiddleName,FamilyName"
                , ",DateOfBirth"
                , ",Mobile,Email"
                , "FROM Person"
                , "ORDER BY FamilyName COLLATE NOCASE ASC"]) []

        return $ map personOfSqlValues valuess


-- | Get the person with the given id.
getPerson  :: IConnection conn => conn -> PersonId -> IO Person
getPerson conn userId
 = do   [values] <- quickQuery' conn (unlines
                [ "SELECT PersonId,MemberId"
                , ",PreferedName,FirstName,MiddleName,FamilyName"
                , ",DateOfBirth"
                , ",Mobile,Email"
                , "FROM  Person"
                , "WHERE PersonId=?" ])
                [ toSql userId ]

        return  $ personOfSqlValues values


-- | Insert a person.
insertPerson :: IConnection conn => conn -> Person -> IO Integer
insertPerson conn person
 = do   stmt    <- prepare conn $ unlines
                [ "INSERT INTO Person"
                , "(MemberId"
                , ",PreferedName,FirstName,MiddleName,FamilyName"
                , ",DateOfBirth"
                , ",Mobile,Email)"
                , "VALUES (?,?,?,?,?,?,?,?)" ]

        execute stmt
                [ toSql (personMemberId      person)
                , toSql (personPreferredName person)
                , toSql (personFirstName     person)
                , toSql (personMiddleName    person)
                , toSql (personFamilyName    person)
                , toSql (personDateOfBirth   person)
                , toSql (personMobile        person)
                , toSql (personEmail         person) ]


-- | Update a person.
updatePerson :: IConnection conn => conn -> Person -> IO Integer
updatePerson conn person
 = do   stmt    <- prepare conn $ unlines
                [ "UPDATE Person"
                , "SET MemberId=?"
                , ",PreferedName=?,FirstName=?,MiddleName=?,FamilyName=?"
                , ",DateOfBirth=?"
                , ",Mobile=?,Email=?"
                , "WHERE PersonId=?"]

        execute stmt
         $      [ toSql (personMemberId      person)
                , toSql (personPreferredName person)
                , toSql (personFirstName     person)
                , toSql (personMiddleName    person)
                , toSql (personFamilyName    person)
                , toSql (personDateOfBirth   person)
                , toSql (personMobile        person)
                , toSql (personEmail         person)
                , toSql (personId            person) ]
