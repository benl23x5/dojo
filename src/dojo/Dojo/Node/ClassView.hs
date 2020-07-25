
module Dojo.Node.ClassView (cgiClassView) where
import Dojo.Data.Session
import Dojo.Data.Class
import Dojo.Framework
import Dojo.Chrome
import Dojo.Paths
import Dojo.Fail
import Config
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | View a single class, using a vertical form.
--      &cid=NAT   View the class with this id.
cgiClassView
        :: Session -> [(String, String)]
        -> CGI CGIResult

cgiClassView ss inputs
 | Just strClassId  <- lookup "cid" inputs
 , Right cid        <- parse strClassId
 = do   conn    <- liftIO $ connectSqlite3 databasePath
        classs  <- liftIO $ getClass conn cid
        liftIO $ disconnect conn

        cgiClassView_page ss classs

 | otherwise
 = throw $ FailNodeArgs "class view" inputs


cgiClassView_page ss classs
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader $ classDisplayName classs
        pageBody
         $ do   tablePaths $ pathsJump ss

                tablePaths
                 [ Path "New Event of Class"
                        cgiName
                        [ ("s",         show $ sessionHash ss)
                        , ("n",         "ee")
                        , ("Location",  maybe "" pretty $ classLocation classs)
                        , ("Type",      maybe "" pretty $ classType classs)
                        , ("Time",      maybe "" pretty $ classTimeStart classs) ]]

                divClassDetails classs


-------------------------------------------------------------------------------
-- | Class Details
divClassDetails :: Class -> Html
divClassDetails classs
 = H.div ! A.class_ "details" ! A.id "class-details-view"
 $ do
        H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "location"; th "day"
                tr $ do td' $ classLocation classs
                        td' $ classDay classs

        H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "start time"; th "end time"
                tr $ do td' $ classTimeStart classs
                        td' $ classTimeEnd classs

        H.table
         $ do   tr $ do th "type"
                tr $ do td' $ classType classs

        H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "first date"; th "final date"
                tr $ do td' $ classDateFirst classs
                        td' $ classDateFinal classs

 where  td' val = td $ H.toMarkup $ maybe "" pretty $ val
