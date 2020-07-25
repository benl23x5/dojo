
module Dojo.Data.Class.Database where
import Dojo.Data.Class.Base
import Dojo.Trivia
import Dojo.Fail
import Dojo.Framework


------------------------------------------------------------------------------
-- | Get all the classes.
getClasses  :: IConnection conn => conn -> IO [Class]
getClasses conn
 = do   vss <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity classEntity
                , "ORDER BY Location, Day ASC"]) []

        return $ map classOfSqlValues vss


-- | Get the class with the given id.
getClass  :: IConnection conn => conn -> ClassId -> IO Class
getClass conn cid
 = do   vss     <- quickQuery' conn (unlines
                [ sqlSelectAllFromEntity classEntity
                , "WHERE ClassId=?" ])
                [ toSql cid ]

        case vss of
         [vs] -> return $ classOfSqlValues vs
         _    -> throw $ FailUnknownEntity "class" (pretty cid)
