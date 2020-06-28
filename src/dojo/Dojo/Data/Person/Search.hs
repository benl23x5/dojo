
module Dojo.Data.Person.Search
        ( Found (..)
        , findPerson)
where
import Dojo.Data.Person.Database
import Dojo.Data.Person.Base
import Dojo.Base
import Data.Function
import qualified Data.Char as Char
import qualified Data.List as List

-- Found ----------------------------------------------------------------------
-- | Find a person based on their name.
data Found a
        -- | Found a single result.
        = FoundOk   a

        -- | Found multiple results for these search parameters.
        | FoundMany [a]

        -- | Didn't find any results.
        | FoundNone
        deriving Show



-- Search ---------------------------------------------------------------------
-- | Find a person based on a search string.
--
--   We assume the search string consists of at least one complete name
--   and some optional initials, eg "john", "john s" or "j smith"
--
--   We first find all people that match any of the words in the search
--   string, then disambiguate them by selecting only people that have a
--   name with a prefix that satisfies all of the words in the search string.
--
--   This sends several queries to the DB to do the search,
--   but we don't have any user load, so don't worry too much.
--
findPerson :: IConnection conn
           => conn -> String -> IO (Found Person)

findPerson conn strSearch
 = do   let strWords    = words strSearch

        psPreferredName
         <- fmap concat
         $  mapM (searchPeopleFromField conn "PreferedName") strWords

        psFirstName
         <- fmap concat
         $  mapM (searchPeopleFromField conn "FirstName")    strWords

        psFamilyName
         <- fmap concat
         $  mapM (searchPeopleFromField conn "FamilyName")   strWords

        let psAll  = concat [psPreferredName, psFirstName, psFamilyName]
        return $ discriminateResults strSearch psAll


-- | Given a search string and a list of candidate person matches,
--   try to select a single person by comparing name prefixes.
discriminateResults :: String -> [Person] -> Found Person
discriminateResults sQuery psMatch
 = let psSat
        = List.nubBy ((==) `on` personId)
        $ [ person  | person <- psMatch
                    , all (isWordSatisfied person) $ words sQuery ]
   in case psSat of
         []   -> case psMatch of
                        []   -> FoundNone
                        [p1] -> FoundOk p1
                        ps   -> FoundMany ps
         [p1] -> FoundOk p1
         ps   -> FoundMany ps


-- | Check if a search keyword is satisfied by a person.
--   This is true if the search keyword is a prefix of any of the person's names.
isWordSatisfied :: Person -> String -> Bool
isWordSatisfied person sWord
 = let  sWord' = map Char.toLower sWord
        sName (PersonName s) = map Char.toLower s
   in   any (isPrefixOf sWord') (words $ sName $ personFirstName person)
     || any (isPrefixOf sWord') (words $ sName $ personPreferredName person)
     || any (isPrefixOf sWord') (words $ sName $ personFamilyName person)


-- | Get the list of personIds that have the given value for a field.
--
--   Hard coded limit to 5 results.
searchPeopleFromField
        :: IConnection conn
        => conn -> String -> String -> IO [Person]

searchPeopleFromField conn fieldName name
 = liftM convertResult
 $ quickQuery' conn (unlines
        [ "SELECT "
        , "PersonId,MemberId,"
        , "PreferedName,FirstName,MiddleName,FamilyName,"
        , "DateOfBirth,Mobile,Email"
        , "FROM   Person"
        , "WHERE  UPPER(" ++ fieldName ++ ")=UPPER(?)"
        , "LIMIT  5"])
        [toSql name]


convertResult :: [[SqlValue]] -> [Person]
convertResult values
 = map personOfSqlValues values


