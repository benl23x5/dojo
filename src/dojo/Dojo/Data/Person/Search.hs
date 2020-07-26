
module Dojo.Data.Person.Search
        ( Found (..)
        , findPerson)
where
import Dojo.Data.Person.Base
import Dojo.Framework
import Dojo.Trivia
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
--   We take a list of ids of people to not consider in the search,
--   perhaps these people are already in the list we are trying to add to.
--
findPerson
        :: IConnection conn
        => conn
        -> String            -- ^ Search string.
        -> [PersonId]        -- ^ Ids of people to not consider.
        -> IO (Found Person)

findPerson conn strSearch pidsSkip
 = do
        -- Find all person rows that have any of the words in the search
        -- string as a prefix of a preferred, first or family name.
        let strWords = words $ map Char.toLower strSearch
        psPreferredName
         <- fmap concat
         $  mapM (searchPeopleFromField conn "PreferredName") strWords

        psFirstName
         <- fmap concat
         $  mapM (searchPeopleFromField conn "FirstName") strWords

        psFamilyName
         <- fmap concat
         $  mapM (searchPeopleFromField conn "FamilyName") strWords

        let psAll  = concat [psPreferredName, psFirstName, psFamilyName]

        -- For people with names like "Paul Paulson", searching for
        --  "Paul" will return duplicate hits, that we then need to
        --  fold up.
        let psNub  = List.nubBy ((==) `on` personId) psAll

        -- Skip results we were told to ignore.
        --  these might already be in the list, and we don't want
        --  to return them in the search again.
        let psCut  = flip filter psNub $ \person ->
                case personId person of
                 Nothing  -> True
                 Just pid -> not $ elem pid pidsSkip

        let cands  = map candidateOfPerson psCut
        let cands' = mapMaybe (filtersCandidate strWords) cands

        return $ case cands' of
         []   -> FoundNone
         [c1] -> FoundOk $ candidatePerson c1
         cs   -> FoundMany $ map candidatePerson cs



-- | Get the list of personIds that have the given value for a field.
--
--   Hard coded limit to 5 results.
searchPeopleFromField
        :: IConnection conn
        => conn -> String -> String -> IO [Person]

searchPeopleFromField conn fieldName name
 = liftM convertResult
 $ quickQuery' conn (unlines
        [ sqlSelectAllFromEntity personEntity
        , "WHERE  LOWER(" ++ fieldName ++ ") LIKE '%' || LOWER(?) || '%'"])
        [toSql name]


convertResult :: [[SqlValue]] -> [Person]
convertResult values
 = map personOfSqlValues values


-- Candidate ------------------------------------------------------------------
-- | A candidate person that we're considering in the search.
--
--   We consider how the search string matches the candidate words,
--   and each time the search string matches one of the candidate words
--   we consider it 'spent' and try to match the others. We do it this way
--
data Candidate
        = Candidate
        { _candidateWords       :: [String]
        , candidatePerson       :: Person }


-- | Construct a candidate from a person.
--   We split each of the names into parts, so full name like
--   "Ricardo de Castro" (Ricardo of-the-castle) can be matched by "ric ca",
--   even though the full family name is "de Castro", which starts with "de".
--
candidateOfPerson :: Person -> Candidate
candidateOfPerson person
 = Candidate
    (concat
        [ words $ sName' $ personPreferredName person
        , words $ sName' $ personFirstName person
        , words $ sName' $ personFamilyName person ])
        person
 where
        sName' (Just (PersonName s))    = map Char.toLower s
        sName' Nothing                  = []



filtersCandidate :: [String] -> Candidate -> Maybe Candidate
filtersCandidate [] c = Just c
filtersCandidate (w : ws) c
 = case filterCandidate w c of
        Nothing -> Nothing
        Just c' -> filtersCandidate ws c'


filterCandidate :: String -> Candidate -> Maybe Candidate
filterCandidate sSearch (Candidate ssName person)
 = go [] ssName
 where  go _acc [] = Nothing

        go acc (sName : ssRest)
         | isPrefixOf sSearch sName
         = Just (Candidate (acc ++ ssRest) person)

         | otherwise
         = go (acc ++ [sName]) ssRest

