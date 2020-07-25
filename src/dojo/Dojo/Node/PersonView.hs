
module Dojo.Node.PersonView where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.Event
import Dojo.Paths
import Dojo.Chrome
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
        conn <- liftIO $ connectSqlite3 databasePath

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
         $ do   let bHasPref = isJust $ personPreferredName person
                tr $ do th "first"
                        when bHasPref $ th "preferred"
                        th "family"
                tr $ do td  $ H.toMarkup $ pretty $ personFirstName person
                        when bHasPref $ td' $ personPreferredName person
                        td' $ personFamilyName   person

        H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "date of birth"; th "home dojo"
                tr $ do td' $ personDateOfBirth  person
                        td' $ personDojoHome person

        H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "membership id"; th "renewal date"
                tr $ do td' $ personMemberId person
                        td' $ personMembershipRenewal person

        H.table
         $ do   tr $ th "membership level"
                tr $ td' $ personMembershipLevel person

        H.table
         $ do   tr $ th "email address"
                tr $ td' $ personEmail person

        H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "mobile phone"; th "fixed phone"
                tr $ do td' $ personPhoneMobile  person
                        td' $ personPhoneFixed person

        H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "emergency contact 1"; th "phone"
                tr $ do td' $ personEmergencyName1 person
                        td' $ personEmergencyPhone1 person

        H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "emergency contact 2"; th "phone"
                tr $ do td' $ personEmergencyName2 person
                        td' $ personEmergencyPhone2 person

 where  td' val = td $ H.toMarkup $ maybe "" pretty $ val


-- | Events that a person attended.
divEventList :: Session -> [Event] -> Html
divEventList ss events
 = H.div ! A.class_ "list person-attendance"
 $ H.table
 $ do
        col' "Date"; col' "Time"; col' "Location"
        tr $ do th "date"; th "time"; th "location"

        forM_ events $ \event -> tr $ do
         td' event $ eventDate event
         td' event $ eventTime event
         td' event $ eventLocation  event

 where  col' c   = col ! A.class_ c

        -- TODO: suppress link on no eid
        pathView event
         = let  Just eid = eventId event
           in   pathEventView ss $ eid

        td' event val
         = td $ (a ! A.href (H.toValue $ pathView event))
                (H.toMarkup $ fromMaybe "" $ fmap pretty val)

