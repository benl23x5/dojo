
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
        Just person  <- liftIO $ getPerson conn pid

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

                -- TODO: add can cancel link if person does not
                -- exist in the database yet.
                tablePaths
                 $ case personId person of
                    Nothing  -> []
                    Just pid -> [pathPersonEdit ss $ Just pid]

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
 = H.div ! A.id "person-details-view" ! A.class_ "details"
 $ do   H.table
         $ do   tr $ do th "first"; th "preferred"; th "family"
                tr $ do td (H.toMarkup $ personFirstName    person)
                        td (H.toMarkup $ personPreferredName person)
                        td (H.toMarkup $ personFamilyName   person)

        H.table
         $ do   tr $ th "date of birth"
                tr $ td (H.toMarkup $ personDateOfBirth  person)

        H.table
         $ do   tr $ th "member id"
                tr $ td (H.toMarkup $ personMemberId person)

        H.table
         $ do   tr $ th "mobile phone number"
                tr $ td (H.toMarkup $ personPhoneMobile  person)

        H.table
         $ do   tr $ th "fixed phone number"
                tr $ td (H.toMarkup $ personPhoneFixed person)

        H.table
         $ do   tr $ th "email address"
                tr $ td (H.toMarkup $ personEmail person)

        H.table
         $ do   tr $ th "home dojo"
                tr $ td (H.toMarkup $ personDojoHome person)

        H.table
         $ do   tr $ th "membership level"
                tr $ td (H.toMarkup $ personMembershipLevel person)

        H.table
         $ do   tr $ th "membership renewal date"
                tr $ td (H.toMarkup $ personMembershipRenewal person)

        H.table
         $ do   tr $ th "emergency contact name 1"
                tr $ td (H.toMarkup $ personEmergencyName1 person)

        H.table
         $ do   tr $ th "emergency contact phone 1"
                tr $ td (H.toMarkup $ personEmergencyPhone1 person)

        H.table
         $ do   tr $ th "emergency contact name 2"
                tr $ td (H.toMarkup $ personEmergencyName2 person)

        H.table
         $ do   tr $ th "emergency contact phone 2"
                tr $ td (H.toMarkup $ personEmergencyPhone2 person)


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

