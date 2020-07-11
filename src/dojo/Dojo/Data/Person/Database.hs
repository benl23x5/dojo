
module Dojo.Data.Person.Database where
import Dojo.Data.Person.Base
import Dojo.Framework
import Dojo.Trivia
import Dojo.Base


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
 = do   vss <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity personEntity
                , "ORDER BY FamilyName COLLATE NOCASE ASC"]) []

        return $ map personOfSqlValues vss


-- | Get the person with the given id.
getPerson  :: IConnection conn => conn -> PersonId -> IO (Maybe Person)
getPerson conn pid
 = do   vss     <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity personEntity
                , "WHERE PersonId=?" ])
                [ toSql pid ]

        case vss of
         [vs] -> return $ Just $ personOfSqlValues vs
         _    -> return Nothing


-- | Insert a person.
insertPerson :: IConnection conn => conn -> Person -> IO Person
insertPerson conn person
 = do   -- Insert all the fields as a new row.
        stmt    <- prepare conn $ sqlInsertAllIntoEntity personEntity
        execute stmt [fieldToSql pf person | pf <- personFieldsNoKey]

        -- Get the primary key of the new row.
        [[v]]   <- quickQuery' conn (unlines
                [ "SELECT last_insert_rowid()"])
                []

        let Right iPersonId = safeConvert v
        return person { personId = iPersonId }


-- | Update a person.
updatePerson :: IConnection conn => conn -> Person -> IO Integer
updatePerson conn person
 = do   -- Fields are values for the columns to update,
        --   then the primary key of the row that we're updating.
        stmt <- prepare conn $ sqlUpdateAllOfEntity personEntity
        execute stmt
         $  [fieldToSql pf person | pf <- personFieldsNoKey]
         ++ [toSql $ personId person]
