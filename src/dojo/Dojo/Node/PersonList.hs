
module Dojo.Node.PersonList where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Paths
import Dojo.Chrome
import Dojo.Base
import Config
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | A table of people, with edit links on the right.
cgiPersonList :: Session -> [(String, String)] -> CGI CGIResult
cgiPersonList sid _inputs
 = do   -- Connect to the database.
        conn    <- liftIO $ connectSqlite3 databasePath

        -- Read current data for all people
        people  <- liftIO $ getPeople conn
        liftIO $ disconnect conn

        cgiPersonList_list sid people

cgiPersonList_list ss people
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "People"
        pageBody
         $ do   tablePaths $ pathsJump ss
                tablePaths $ [pathPersonAdd ss]
                tablePeople ss people


-- | Build the person table.
tablePeople :: Session -> [Person] -> H.Html
tablePeople ss people
 = H.div ! A.class_ "list person-list"
 $ H.table
 $ do   col ! A.class_ "ShortName"
        col ! A.class_ "FamilyName"
        col ! A.class_ "PhoneMobile"
        tr $ do th "pref / first"; th "family"; th "phone"

        mapM_ (trPerson ss) people


-- | Build one row of the person table.
trPerson :: Session -> Person -> H.Html
trPerson ss person
 = tr
 $ do   -- Clicking on any column takes us to the person view page.
        -- TODO: suppress link on no personid
        let Just pid = personId person
        let pathView = pathPersonView ss pid

        -- Person data.
        let tdField val
             = td $ (a ! A.href (H.toValue pathView))
                    (H.toMarkup val)

        tdField (personShortName person)
        tdField (personFamilyName person)
        tdField (personPhoneMobile person)

