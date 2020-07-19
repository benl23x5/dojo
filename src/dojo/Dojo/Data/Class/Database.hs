
module Dojo.Data.Class.Database where
import Dojo.Data.Class.Base
import Dojo.Framework


-- | Get all the classes.
getClasses  :: IConnection conn => conn -> IO [Class]
getClasses conn
 = do   vss <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity classEntity ]) []

        return $ map classOfSqlValues vss

