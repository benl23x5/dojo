
module Dojo.Mode.ListPeople where
import Dojo.Config
import Dojo.Framework
import Dojo.Data


-- | Extract a list of all people in the database.
modeListPeople :: Config -> IO ()
modeListPeople cc
 = do   conn    <- liftIO $ connectSqlite3 $ configPathDatabase cc
        ps      <- liftIO $ getPeople conn
        putStrLn $ showPeople ps


showPeople :: [Person] -> String
showPeople ps
 = intercalate "\n"
 $ map showPerson ps


showPerson :: Person -> String
showPerson p
 = intercalate ","
 [ show $ personFirstName p
 , show $ personFamilyName p]
