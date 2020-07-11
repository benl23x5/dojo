
module Dojo.Data.Person.Database where
import Dojo.Data.Person.Base
import Dojo.Framework
import Dojo.Base


-- toSql ----------------------------------------------------------------------
instance Convertible PersonId   SqlValue where
 safeConvert (PersonId n)               = safeConvert n

instance Convertible PersonMemberId SqlValue where
 safeConvert (PersonMemberId mid)       = safeConvert mid

instance Convertible PersonName  SqlValue where
 safeConvert (PersonName name)          = safeConvert name

instance Convertible PersonDate  SqlValue where
 safeConvert (PersonDate dob)           = safeConvert dob

instance Convertible PersonPhone SqlValue where
 safeConvert (PersonPhone mobile)       = safeConvert mobile

instance Convertible PersonEmail  SqlValue where
 safeConvert (PersonEmail  email)       = safeConvert email

instance Convertible PersonDojo SqlValue where
 safeConvert (PersonDojo mobile)        = safeConvert mobile

instance Convertible PersonMembershipLevel SqlValue where
 safeConvert (PersonMembershipLevel mobile) = safeConvert mobile


-- fromSql --------------------------------------------------------------------
instance Convertible SqlValue PersonId where
 safeConvert val        = liftM PersonId (safeConvert val)

instance Convertible SqlValue PersonMemberId where
 safeConvert val        = liftM PersonMemberId (safeConvert val)

instance Convertible SqlValue PersonName where
 safeConvert val        = liftM PersonName (safeConvert val)

instance Convertible SqlValue PersonDate where
 safeConvert val        = liftM PersonDate (safeConvert val)

instance Convertible SqlValue PersonPhone where
 safeConvert val        = liftM PersonPhone (safeConvert val)

instance Convertible SqlValue PersonEmail where
 safeConvert val        = liftM PersonEmail (safeConvert val)

instance Convertible SqlValue PersonDojo where
 safeConvert val        = liftM PersonDojo (safeConvert val)

instance Convertible SqlValue PersonMembershipLevel where
 safeConvert val        = liftM PersonMembershipLevel (safeConvert val)


-------------------------------------------------------------------------------
-- | Build a person from a list of Sql values.
personOfSqlValues :: [SqlValue] -> Person
personOfSqlValues
        [ pid, memberId
        , preferredName, firstName, familyName
        , dateOfBirth
        , phoneMobile, phoneFixed, email
        , dojoHome
        , memberLevel, memberDate
        , emergencyName1, emergencyPhone1
        , emergencyName2, emergencyPhone2 ]

        = Person
        { personId                      = fromSql pid
        , personMemberId                = fromSql memberId
        , personPreferredName           = fromSql preferredName
        , personFirstName               = fromSql firstName
        , personFamilyName              = fromSql familyName
        , personDateOfBirth             = fromSql dateOfBirth
        , personPhoneMobile             = fromSql phoneMobile
        , personPhoneFixed              = fromSql phoneFixed
        , personEmail                   = fromSql email
        , personDojoHome                = fromSql dojoHome
        , personMembershipLevel         = fromSql memberLevel
        , personMembershipRenewal       = fromSql memberDate
        , personEmergencyName1          = fromSql emergencyName1
        , personEmergencyPhone1         = fromSql emergencyPhone1
        , personEmergencyName2          = fromSql emergencyName2
        , personEmergencyPhone2         = fromSql emergencyPhone2
        }

personOfSqlValues _ = error "personOfValues: no match"


-- | Get all the people, ordered by family name.
getPeople  :: IConnection conn => conn -> IO [Person]
getPeople conn
 = do   valuess <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity personEntity
                , "ORDER BY FamilyName COLLATE NOCASE ASC"]) []

        return $ map personOfSqlValues valuess


-- | Get the person with the given id.
getPerson  :: IConnection conn => conn -> PersonId -> IO (Maybe Person)
getPerson conn pid
 = do   valuess <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity personEntity
                , "WHERE PersonId=?" ])
                [ toSql pid ]

        case valuess of
         [vs] -> return $ Just $ personOfSqlValues vs
         _    -> return Nothing


-- | Insert a person.
insertPerson :: IConnection conn => conn -> Person -> IO Person
insertPerson conn person
 = do   stmt    <- prepare conn $ sqlInsertAllIntoEntity personEntity
        execute stmt
                [ toSql (personMemberId          person)
                , toSql (personPreferredName     person)
                , toSql (personFirstName         person)
                , toSql (personFamilyName        person)
                , toSql (personDateOfBirth       person)
                , toSql (personPhoneMobile       person)
                , toSql (personPhoneFixed        person)
                , toSql (personEmail             person)
                , toSql (personDojoHome          person)
                , toSql (personMembershipLevel   person)
                , toSql (personMembershipRenewal person)
                , toSql (personEmergencyName1    person)
                , toSql (personEmergencyPhone1   person)
                , toSql (personEmergencyName2    person)
                , toSql (personEmergencyPhone2   person)]

        [[v]]   <- quickQuery' conn (unlines
                [ "SELECT last_insert_rowid()"])
                []

        let Right iPersonId = safeConvert v
        return person { personId = iPersonId }


-- | Update a person.
updatePerson :: IConnection conn => conn -> Person -> IO Integer
updatePerson conn person
 = do   stmt    <- prepare conn $ sqlUpdateAllOfEntity personEntity
        execute stmt
                [ toSql (personMemberId                 person)
                , toSql (personPreferredName            person)
                , toSql (personFirstName                person)
                , toSql (personFamilyName               person)
                , toSql (personDateOfBirth              person)
                , toSql (personPhoneMobile              person)
                , toSql (personPhoneFixed               person)
                , toSql (personEmail                    person)
                , toSql (personDojoHome                 person)
                , toSql (personMembershipLevel          person)
                , toSql (personMembershipRenewal        person)
                , toSql (personEmergencyName1           person)
                , toSql (personEmergencyPhone1          person)
                , toSql (personEmergencyName2           person)
                , toSql (personEmergencyPhone2          person)
                , toSql (personId                       person) ]
