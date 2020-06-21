
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
        :: Session -> Person -> [Event] -> CGI CGIResult

cgiPersonView_page ss person events
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader $ personDisplayName person
        pageBody
         $ do   tablePaths $ pathsJump ss
                tablePaths $ [pathPersonEdit ss $ Just $ personId person]
                divPersonView ss person events


-- Person ---------------------------------------------------------------------
divPersonView :: Session -> Person -> [Event] -> Html
divPersonView ss person events
 = H.div   ! A.id "person-view"
 $ do
        divPersonDetails person
        divEventList ss events


-- | Person Details
divPersonDetails :: Person -> Html
divPersonDetails person
 = H.div ! A.id "person-details-view"
 $ do   H.table
         $ do   tr $ do th "first"; th "pref"; th "middle"; th "family"
                tr $ do td (H.toMarkup $ personFirstName    person)
                        td (H.toMarkup $ personPreferredName person)
                        td (H.toMarkup $ personMiddleName   person)
                        td (H.toMarkup $ personFamilyName   person)

        H.table
         $ do   tr $ do th "dob"; th "member id"
                tr $ do td (H.toMarkup $ personDateOfBirth  person)
                        td (H.toMarkup $ personMemberId person)

        H.table
         $ do   tr $ do th "mobile"; th "email"
                tr $ do td (H.toMarkup $ personMobile   person)
                        td (H.toMarkup $ personEmail    person)


-- | Events that a person attended.
divEventList :: Session -> [Event] -> Html
divEventList ss events
 = H.div ! A.class_ "list person-attendance"
 $ H.table
 $ do   col' "Date"; col' "Time"; col' "Location"
        tr $ do th "date"; th "time"; th "location"

        forM_ events $ \event -> tr $ do
         td' event (eventDate      event)
         td' event (eventTime      event)
         td' event (eventLocation  event)

 where  col' c   = col ! A.class_ c

        pathView event
         = pathEventView ss $ eventId event

        td' event val
         = td $ (a ! A.href (H.toValue $ pathView event))
                (H.toMarkup val)

