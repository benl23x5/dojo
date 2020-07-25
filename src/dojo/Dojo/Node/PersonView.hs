
module Dojo.Node.PersonView (cgiPersonView) where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.Event
import Dojo.Chrome
import Dojo.Paths
import Dojo.Fail
import Config
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | View a single person, using a vertical form.
--      &pid=NAT   View the person with this id.
cgiPersonView
        :: Session -> [(String, String)]
        -> CGI CGIResult

cgiPersonView ss inputs
 | Just strPersonId <- lookup "pid" inputs
 , Right pid        <- parse strPersonId
 = do   conn    <- liftIO $ connectSqlite3 databasePath
        person  <- liftIO $ getPerson conn pid
        events  <- liftIO $ getAttendanceOfPersonId conn pid
        liftIO $ disconnect conn

        cgiPersonView_page ss person events

 | otherwise
 = throw $ FailNodeArgs "person view" inputs


cgiPersonView_page ss person events
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader $ personDisplayName person
        pageBody
         $ do   tablePaths $ pathsJump ss
                tablePaths [pathPersonEdit ss $ personId person]
                H.div ! A.id "person-view"
                 $ do   divPersonDetails person
                        divEventList ss events


-------------------------------------------------------------------------------
-- | Person Details
divPersonDetails :: Person -> Html
divPersonDetails person
 = H.div ! A.class_ "details" ! A.id "person-details-view"
 $ do
        H.table
         $ do   let bHasPref = isJust $ personPreferredName person

                (if bHasPref
                 then do col ! A.class_ "Col3A"
                         col ! A.class_ "Col3B"
                         col ! A.class_ "Col3C"
                 else do col ! A.class_ "Col2A"
                         col ! A.class_ "Col2B")

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

        -- suppress emergency contact if not filled.
        when (or [ isJust $ personEmergencyName1  person
                 , isJust $ personEmergencyPhone1 person])
         $ H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "emergency contact 1"; th "phone"
                tr $ do td' $ personEmergencyName1 person
                        td' $ personEmergencyPhone1 person


        -- suppress emergency contact if not filled.
        when (or [ isJust $ personEmergencyName2  person
                 , isJust $ personEmergencyPhone2 person])
         $ H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "emergency contact 2"; th "phone"
                tr $ do td' $ personEmergencyName2 person
                        td' $ personEmergencyPhone2 person

 where  td' val = td $ H.toMarkup $ maybe "" pretty $ val


-------------------------------------------------------------------------------
-- | Events that a person has attended.
divEventList :: Session -> [Event] -> Html
divEventList ss events
 = H.div ! A.class_ "list" ! A.id "person-attendance"
 $ H.table
 $ do
        col ! A.class_ "Date"
        col ! A.class_ "Time"
        col ! A.class_ "Location"
        tr $ do th "date"; th "time"; th "location"

        -- Clicking any column goes to event view page.
        forM_ events $ \event -> tr $ do
         td' event $ eventDate event
         td' event $ eventTime event
         td' event $ eventLocation  event

 where  td' event val
         | Just eid <- eventId event
         = td $ (a ! A.href (H.toValue $ pathEventView ss eid))
                (H.toMarkup $ fromMaybe "" $ fmap pretty val)

         | otherwise
         = td $ (H.toMarkup $ fromMaybe "" $ fmap pretty val)

