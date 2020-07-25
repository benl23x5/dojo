
module Dojo.Data.Dojo.Database where
import Dojo.Framework
import Dojo.Trivia

-- | Get all the Dojo names, orderd alphabetically.
getDojos :: IConnection conn => conn -> IO [PersonDojo]
getDojos conn
 = do   vss <- quickQuery' conn (unlines
                [ "SELECT Name FROM v1_Dojo"
                , "ORDER BY NAME ASC"]) []

        let parseDojo [v]
             = let Just dojo = fromSql v
               in  dojo

        let dojos = map parseDojo vss
        return dojos

