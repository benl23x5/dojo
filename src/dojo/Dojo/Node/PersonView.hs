
module Dojo.Node.PersonView (cgiPersonView) where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.Event
import Dojo.Chrome
import Dojo.Paths
import Dojo.Fail
import Dojo.Framework
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
 , Right pid    <- parse strPersonId
 = do   conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        person  <- liftIO $ getPerson conn pid
        events  <- liftIO $ getAttendanceOfPersonId conn pid
        liftIO $ disconnect conn

        let sName = fromMaybe "[person]" $ personAliasName person
        cgiPageNavi ss "People" sName (pathsJump ss)
         $ H.div ! A.class_ "person-view"
         $ do
                H.div ! A.class_ "person-alias-name"
                 $ H.table $ H.tr $ H.td $ H.string sName

                let bCanDel = sessionIsAdmin ss && null events

                tableActions
                 $  [pathPersonEdit ss $ personId person]
                 ++ (if bCanDel  then [pathPersonDel  ss $ pid] else [])
                 ++ [pathPersonDevLink ss pid]

                H.div ! A.class_ "details"
                 $ do   divPersonDetails         ss person
                        divPersonEventSummary    ss events
                        divPersonEventAttendance ss events

cgiPersonView _ inputs
 = throw $ FailNodeArgs "person view" inputs


-------------------------------------------------------------------------------
-- | Person Details
divPersonDetails :: Session -> Person -> Html
divPersonDetails ss person
 = do
        let isAdmin     = sessionIsAdmin ss

        H.table
         $ do   let bHasPref = isJust $ personPreferredName person

                (if bHasPref
                 then do H.col ! A.class_ "Col3A"
                         H.col ! A.class_ "Col3B"
                         H.col ! A.class_ "Col3C"
                 else do H.col ! A.class_ "Col2A"
                         H.col ! A.class_ "Col2B")

                H.tr $ do H.th "first"
                          when bHasPref $ H.th "preferred"
                          H.th "family"

                H.tr $ do td' $ personFirstName person
                          when bHasPref $ td' $ personPreferredName person
                          td' $ personFamilyName   person

        -- Suppress date of birth if the role is less than Admin.
        H.table
         $ do   H.col ! A.class_ "Col2A"; H.col ! A.class_ "Col2B"
                H.tr $ do H.th "home dojo"
                          when isAdmin $ H.th "date of birth"
                H.tr $ do td' $ personDojoHome person
                          when isAdmin $ td' $ personDateOfBirth person

        H.table
         $ do   H.col ! A.class_ "Col2A"; H.col ! A.class_ "Col2B"
                H.tr $ do H.th "membership id"; H.th "renewal date"
                H.tr $ do td' $ personMemberId person
                          td' $ personMembershipRenewal person

        -- Keep this on a single line as the values are long.
        H.table
         $ do   H.tr $ H.th "membership level"
                H.tr $ td' $ personMembershipLevel person

        -- Keep this on a single line as the values are long.
        H.table
         $ do   H.tr $ H.th "email address"
                H.tr $ td' $ personEmail person

        let hasPhoneFixed = isJust $ personPhoneFixed person
        H.table
         $ do   H.col ! A.class_ "Col2A"; H.col ! A.class_ "Col2B"
                H.tr $ do H.th "mobile phone"
                          when hasPhoneFixed $ H.th "fixed phone"
                H.tr $ do td' $ personPhoneMobile  person
                          when hasPhoneFixed $ td' $ personPhoneFixed person

        -- Suppress emergency contact if not filled.
        when (or [ isJust $ personEmergencyName1  person
                 , isJust $ personEmergencyPhone1 person])
         $ H.table
         $ do   H.col ! A.class_ "Col2A"; H.col ! A.class_ "Col2B"
                H.tr $ do H.th "emergency contact 1"; H.th "phone"
                H.tr $ do td' $ personEmergencyName1 person
                          td' $ personEmergencyPhone1 person


        -- Suppress emergency contact if not filled.
        when (or [ isJust $ personEmergencyName2  person
                 , isJust $ personEmergencyPhone2 person])
         $ H.table
         $ do   H.col ! A.class_ "Col2A"; H.col ! A.class_ "Col2B"
                H.tr $ do H.th "emergency contact 2"; H.th "phone"
                H.tr $ do td' $ personEmergencyName2 person
                          td' $ personEmergencyPhone2 person

 where  td' val = H.td $ H.toMarkup $ maybe "" pretty $ val


-------------------------------------------------------------------------------
-- | Summary of how many events of each type a person has attended.
divPersonEventSummary :: Session -> [Event] -> Html
divPersonEventSummary _ss events
 = H.div ! A.class_ "list event-summary"
 $ H.table
 $ do   H.col ! A.class_ "Type"
        H.col ! A.class_ "Count"
        H.tr $ do H.th "event type"; H.th "attended"

        forM_ (summarizeEventTypes events)
         $ \((EventType name, nCount)) -> H.tr $ do
                H.td (H.string name)
                H.td (H.string $ show nCount)


-------------------------------------------------------------------------------
-- | List of events that a person has attended.
divPersonEventAttendance :: Session -> [Event] -> Html
divPersonEventAttendance ss events
 = H.div ! A.class_ "list event-attendance"
 $ H.table
 $ do
        H.col ! A.class_ "Date"
        H.col ! A.class_ "Time"
        H.col ! A.class_ "Location"
        H.tr $ do H.th "date"; H.th "time"; H.th "location"

        -- Clicking any column goes to event view page.
        forM_ events $ \event -> H.tr $ do
         td' event $ eventDate event
         td' event $ eventTime event
         td' event $ eventLocation  event

 where  td' event val
         | Just eid <- eventId event
         = H.td $ (H.a ! A.href (H.toValue $ pathEventView ss eid))
                (H.toMarkup $ fromMaybe "" $ fmap pretty val)

         | otherwise
         = H.td $ (H.toMarkup $ fromMaybe "" $ fmap pretty val)

