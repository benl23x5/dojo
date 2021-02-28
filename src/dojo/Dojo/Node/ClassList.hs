
module Dojo.Node.ClassList where
import Dojo.Data.Session
import Dojo.Data.Class
import Dojo.Framework
import Dojo.Paths
import Dojo.Chrome
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | A table of classes.
cgiClassList :: Session -> [(String, String)] -> CGI CGIResult
cgiClassList ss _inputs
 = do   conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        classes <- liftIO $ getClasses conn
        liftIO $ disconnect conn

        cgiPageNavi ss "Classes" "Classes" (pathsJump ss)
         $ divClassList ss classes


-- | Build the class table.
divClassList :: Session -> [Class] -> H.Html
divClassList  ss classes
 = H.div ! A.class_ "list class-list"
 $ H.table
 $ do   H.col ! A.class_ "Location"
        H.col ! A.class_ "Day"
        H.col ! A.class_ "TimeStart"
        H.col ! A.class_ "Type"
        H.tr $ do H.th "location"; H.th "day"; H.th "time"; H.th "type"

        forM_ classes $ \classs -> H.tr $ do
         td' classs $ classLocation classs
         td' classs $ classDay classs
         td' classs $ classTimeStart classs
         td' classs $ classType classs

 where  td' classs val
         | Just cid <- classId classs
         = H.td $ (H.a ! A.href (H.toValue $ pathClassView ss cid))
                  (H.toMarkup $ fromMaybe "" $ fmap pretty val)

         | otherwise
         = H.td $ (H.toMarkup $ fromMaybe "" $ fmap pretty val)

