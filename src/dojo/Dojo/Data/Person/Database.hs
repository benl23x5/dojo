
module Dojo.Data.Person.Database where
import Dojo.Data.Class.Registration
import Dojo.Data.Person.Base
import Dojo.Framework
import Dojo.Trivia
import Dojo.Fail
import qualified Data.Time      as Time


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
        stmt     <- prepare conn $ sqlInsertAllIntoEntity personEntity
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


-- | Delete a person.
deletePerson :: IConnection conn => conn -> Person -> IO Integer
deletePerson conn person
 = do   stmt    <- prepare conn $ unlines
                [ "DELETE FROM v1_Person"
                , "WHERE PersonId=?" ]

        execute stmt
                [ toSql (personId person) ]


------------------------------------------------------------------------------
getMembershipLevels
        :: IConnection conn => conn -> IO [PersonMembershipLevel]
getMembershipLevels conn
 = do   vss <- quickQuery' conn (unlines
                [ "SELECT Name FROM v1_PersonMembershipLevel"
                , "ORDER BY SortOrder ASC" ]) []

        let parseLevel [v]
             | Just level <- fromSql v
             = level
            parseLevel _ = error "parseLevel: no match"

        return $ map parseLevel vss


------------------------------------------------------------------------------
-- | Lookup the device registration code for a given person,
--   or create one if there isn't one listed already.
acquirePersonDeviceRegCode
        :: IConnection conn
        => conn -> String -> PersonId -> IO String

acquirePersonDeviceRegCode conn sSalt pid
 = goLookup
 where
  -- Try to get any existing person device code.
  goLookup
   = do vssCode <- quickQuery' conn (unlines
                [ "SELECT Content FROM v1_PersonDeviceRegCode"
                , "WHERE PersonId=?"
                , "  AND Active=1"
                , "ORDER BY TimeCreated DESC" ])
                [toSql pid]

        let parseCode [v]
             | Just (sCode :: String) <- fromSql v = sCode
            parseCode _ = error "parseCode: no match"

        case map parseCode vssCode of
         sCode : _  -> return sCode
         _          -> goCreate

  -- Create a fresh person device code.
  goCreate
   = do sCode    <- newRandomDeviceCode sSalt
        ztime    <- Time.getZonedTime
        let ltime = Time.zonedTimeToLocalTime ztime

        stmt    <- prepare conn $ unlines
                [ "INSERT INTO v1_PersonDeviceRegCode"
                , "(TimeCreated, PersonId, Content, Active)"
                , "VALUES (?, ?, ?, ?)" ]

        execute stmt
                [ toSql ltime
                , toSql pid
                , toSql sCode
                , toSql (1 :: Integer)]

        return sCode


-- | Lookup the person id mapped to this person device reg code.
lookupPersonIdOfDeviceRegCode
        :: IConnection conn
        => conn -> String -> IO (Maybe PersonId)

lookupPersonIdOfDeviceRegCode conn sCode
 = do
        vssId   <- quickQuery' conn (unlines
                [ "SELECT PersonId FROM v1_PersonDeviceRegCode"
                , "WHERE Content=?"
                , "  AND Active=1"
                , "ORDER BY TimeCreated DESC" ])
                [ toSql sCode ]

        if | [[v]] <- vssId
           , Just (pid :: PersonId) <- fromSql v
           -> return $ Just pid

           | otherwise
           -> return $ Nothing




















