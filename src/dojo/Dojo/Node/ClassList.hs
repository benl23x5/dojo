
module Dojo.Node.ClassList where
import Dojo.Data.Session
import Dojo.Data.Class
import Dojo.Framework
import Dojo.Paths
import Dojo.Chrome
import Config
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | A table of classes.
cgiClassList :: Session -> [(String, String)] -> CGI CGIResult
cgiClassList sid _inputs
 = do   -- Connect to the database.
        conn    <- liftIO $ connectSqlite3 databasePath

        -- Read current data for all people
        classes <- liftIO $ getClasses conn
        liftIO $ disconnect conn

        cgiClassList_list sid classes


cgiClassList_list ss classes
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Classes"
        pageBody
         $ do   tablePaths $ pathsJump ss
                divClassList ss classes


-- | Build the class table.
divClassList :: Session -> [Class] -> H.Html
divClassList  ss classes
 = H.div ! A.class_ "list class-list"
 $ H.table
 $ do   col ! A.class_ "Location"
        col ! A.class_ "Day"
        col ! A.class_ "TimeStart"
        col ! A.class_ "Type"
        tr $ do th "location"; th "day"; th "time"; th "type"

        forM_ classes $ \classs -> tr $ do
         td' classs $ classLocation classs
         td' classs $ classDay classs
         td' classs $ classTimeStart classs
         td' classs $ classType classs

 where  td' classs val
         | Just cid <- classId classs
         = td $ (a ! A.href (H.toValue $ pathClassView ss cid))
                (H.toMarkup $ fromMaybe "" $ fmap pretty val)

         | otherwise
         = td $ (H.toMarkup $ fromMaybe "" $ fmap pretty val)

