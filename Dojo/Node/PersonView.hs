
module Dojo.Node.PersonView where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.Event
import Dojo.Paths
import Dojo.Chrome
import Dojo.Framework
import Dojo.Base
import Config
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | View a single person, using a vertical form.
--      &personId=INT   View the person with this id.
cgiPersonView
        :: Session
        -> [(String, String)]
        -> CGI CGIResult

cgiPersonView ss inputs
 | Just strPersonId     <- lookup "pid" inputs
 , Right pid            <- parse strPersonId
 = do   
        -- Connect to the database.
        conn    <- liftIO $ connectSqlite3 databasePath

        -- Read the current data for the user.
        person  <- liftIO $ getPerson conn pid

        -- Get the events they attended.
        events  <- liftIO $ getAttendanceOfPersonId conn pid
        liftIO $ disconnect conn

        cgiPersonView_page ss person events

 | otherwise
 = error $ "cgiPersonView: bad inputs" ++ show inputs


cgiPersonView_page 
        :: Session
        -> Person
        -> [Event]
        -> CGI CGIResult

cgiPersonView_page ss person events
 = outputFPS $ renderHtml 
 $ H.docTypeHtml
 $ do   pageHeader $ personDisplayName person
        pageBody
         $ do   H.h1 (H.toMarkup $ personDisplayName person)
                tablePaths (pathsJump ss ++ [pathPersonEdit ss $ personId person])
                H.br

                divPersonView ss person events


-- Person ---------------------------------------------------------------------
divPersonView :: Session -> Person -> [Event] -> Html
divPersonView ss person events
 = H.div   ! A.id "person-view"
 $ do   
        divPersonDetails person
        br

        divEventList ss events


-- | Person Details
divPersonDetails :: Person -> Html
divPersonDetails person
 = H.div ! A.id "person-details-view"
 $ do   H.table
         $ do   tr $ do suppressOn (pretty $ personPreferedName person) 
                                   (th "prefered")

                        th "first"

                        suppressOn (pretty $ personMiddleName person)   
                                   (th "middle")

                        th "family"

                        suppressOn (pretty $ personDateOfBirth person)
                                   (th "date of birth")


                tr $ do suppressOn (pretty $ personPreferedName person) 
                                   (td (H.toMarkup $ personPreferedName person))

                        td (H.toMarkup $ personFirstName    person)

                        suppressOn (pretty $ personMiddleName person) 
                                   (td (H.toMarkup $ personMiddleName   person))

                        td (H.toMarkup $ personFamilyName   person)

                        suppressOn (pretty $ personDateOfBirth person)
                                   (td (H.toMarkup $ personDateOfBirth  person))
        br
        H.table
         $ do   tr $ do th "member"
                        th "mobile"
                        th "email"

                tr $ do td (H.toMarkup $ personMemberId person)
                        td (H.toMarkup $ personMobile   person)
                        td (H.toMarkup $ personEmail    person)

 where  suppressOn str action
         = if str == "" then return () else action


-- | Events that a person attended.
divEventList :: Session -> [Event] -> Html
divEventList ss events
 = H.div ! A.class_ "list person-attendance"
 $ H.table
 $ do   col' "Date"
        col' "Time"
        col' "Location"
        col' "Type"
        col' "actions"

        tr $ do th "date"
                th "time"
                th "location"
                th "type"

        mapM_ (trEvent ss) events

 where col' c   = col ! A.class_ c


-- | A single event that a person attended.
trEvent :: Session -> Event -> Html
trEvent ss event
 = tr
 $ do   -- Event data.
        td' (eventDate      event)
        td' (eventTime      event)
        td' (eventLocation  event)
        td' (eventType      event)

        -- View links.
        td $ a  ! A.href  (H.toValue pathView)
                ! A.class_ "link"
                $ "view"

 where  -- Clicking on any column takes us to the event view page.
        pathView        = pathEventView ss $ eventId event
        td' val         = td $ (a ! A.href (H.toValue pathView))
                               (H.toMarkup val) 

