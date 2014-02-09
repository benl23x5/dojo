
module Dojo.Data.Person.Search
        ( Found (..)
        , findPerson)
where
import Dojo.Data.Person.Database
import Dojo.Data.Person.Base
import Dojo.Base
import qualified Data.Set       as Set


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


-- | Apply a function to any value carried in a `Found`
mapFoundIO  :: (a -> IO b) -> Found a -> IO (Found b)
mapFoundIO f ff
 = case ff of
        FoundOk x       
         -> do  x'      <- f x
                return  $ FoundOk x'

        FoundMany xs
         -> do  xs'     <- mapM f xs
                return  $ FoundMany xs'

        FoundNone
         -> return FoundNone


-- Search ---------------------------------------------------------------------
-- | Find a person based on a search string.
findPerson
        :: IConnection conn 
        => conn -> String -> IO (Found Person)

findPerson conn strSearch
 = do   let strWords    = words strSearch

        hitssPreferedName <- mapM (searchPeopleFromField conn "PreferedName") strWords
        hitssFirstName    <- mapM (searchPeopleFromField conn "FirstName")    strWords
        hitssFamilyName   <- mapM (searchPeopleFromField conn "FamilyName")   strWords

        let pidFound     = discriminateResults 
                                hitssPreferedName
                                hitssFirstName
                                hitssFamilyName

        mapFoundIO (getPerson conn) pidFound


discriminateResults 
        :: [[PersonId]]         -- ^ Hits from prefered names.
        -> [[PersonId]]         -- ^ Hits from first names.
        -> [[PersonId]]         -- ^ Hits from family names.
        -> Found PersonId

discriminateResults hitsPrefered hitsFirst hitsFamily
 = let  allPids = Set.unions 
                [ Set.unions $ map Set.fromList hitsPrefered
                , Set.unions $ map Set.fromList hitsFirst
                , Set.unions $ map Set.fromList hitsFamily ]

        result 
         | Set.size allPids == 0
         = FoundNone

         | Set.size allPids == 1
         = let  [pid]   = Set.toList allPids
           in   FoundOk pid

         -- TODO: discriminate based on names.
         | otherwise
         = FoundMany (Set.toList allPids)

   in result


-- | Get the list of personIds that have the given value for a field.
searchPeopleFromField
        :: IConnection conn
        => conn -> String -> String -> IO [PersonId]

searchPeopleFromField conn fieldName name
        = liftM convertResult
        $ quickQuery' conn (unlines
                [ "SELECT PersonId"
                , "FROM   Person"
                , "WHERE  UPPER(" ++ fieldName ++ ")=UPPER(?)"])
                [toSql name]


convertResult :: [[SqlValue]] -> [PersonId]
convertResult valuess
 = map (\vs -> case vs of
                [spid]  -> fromSql spid
                _       -> error "convertResult: unexpected result") valuess
 

