
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
--        classes  <- liftIO $ getClasses conn
        classes <- return []
        liftIO $ disconnect conn

        cgiClassList_list sid classes


cgiClassList_list ss classes
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Classes"
        pageBody
         $ do   tablePaths $ pathsJump ss
                tableClasses ss classes


-- | Build the class table.
tableClasses :: Session -> [Class] -> H.Html
tableClasses ss classes
 = H.div ! A.class_ "list class-list"
 $ H.table
 $ do   col ! A.class_ "Location"
        col ! A.class_ "Type"
        col ! A.class_ "Day"
        col ! A.class_ "Time"
        tr $ do th "location"; th "type"; th "day"; th "time"

        mapM_ (trClass ss) classes


-- | Build one row of the class table.
trClass :: Session -> Class -> H.Html
trClass _ss classs
 = tr
 $ do   -- Clicking on any column takes us to the person view page.
        -- TODO: suppress link on no personid
--        let Just pid = personId person
--        let pathView = pathPersonView ss pid

        -- Person data.
        let tdField val
             = td $ -- (a ! A.href (H.toValue pathView))
                    (H.toMarkup val)

        tdField (classLocation classs)
        tdField (classType classs)
        tdField (classDay classs)
        tdField (classTime classs)


