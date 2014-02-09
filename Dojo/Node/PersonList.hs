
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
         $ do   H.h1 "People"
                tablePaths (pathsJump ss ++ [pathPersonAdd ss])
                H.br

                tablePeople ss people


-- | Build the person table.
tablePeople :: Session -> [Person] -> H.Html
tablePeople ss people
 = H.div ! A.class_ "list person-list"
 $ H.table
 $ do   col ! A.class_ "FirstName"
        col ! A.class_ "FamilyName"
        col ! A.class_ "Mobile"
        col ! A.class_ "Email"
        col ! A.class_ "MemberId"
        col ! A.class_ "actions"

        tr $ do th "first"
                th "family"
                th "mobile"
                th "email"
                th "member"

        mapM_ (trPerson ss) people


-- | Build one row of the person table.
trPerson :: Session -> Person -> H.Html
trPerson ss person
 = tr
 $ do   -- Clicking on any column takes us to the person view page.
        let pathView     
                = pathPersonView ss $ personId person

        -- Person data.
        let tdField val
             = td $ (a ! A.href (H.toValue pathView))
                    (H.toMarkup val)

        tdField (personFirstName   person)  
        tdField (personFamilyName  person)  
        tdField (personMobile      person)  
        tdField (personEmail       person)  
        tdField (personMemberId    person)  

        -- View links.
        td $    a ! A.href (H.toValue pathView)
                  ! A.class_ "link"
                  $ "view"

