
module Dojo.Data.Person.Database where
import Dojo.Data.Person.Base
import Dojo.Framework
import Dojo.Trivia
import Dojo.Fail


------------------------------------------------------------------------------
-- | Get all the people, ordered by family name.
getPeople  :: IConnection conn => conn -> IO [Person]
getPeople conn
 = do   vss <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity personEntity
                , "ORDER BY FamilyName COLLATE NOCASE ASC"]) []

        return $ map personOfSqlValues vss


-- | Get the person with the given id.
getPerson  :: IConnection conn => conn -> PersonId -> IO Person
getPerson conn pid
 = do   vss     <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity personEntity
                , "WHERE PersonId=?" ])
                [ toSql pid ]

        case vss of
         [vs] -> return $ personOfSqlValues vs
         _    -> throw $ FailUnknownEntity "person" (pretty pid)


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


------------------------------------------------------------------------------
getMembershipLevels
        :: IConnection conn => conn -> IO [PersonMembershipLevel]
getMembershipLevels conn
 = do   vss <- quickQuery' conn (unlines
                [ "SELECT Name FROM v1_PersonMembershipLevel"
                , "ORDER BY SortOrder ASC" ]) []

        let parseLevel [v]
             = let Just level = fromSql v
               in  level

        return $ map parseLevel vss
