
module Dojo.Node.PersonEdit.Form
        (formPerson)
where
import Dojo.Data.Person
import Dojo.Framework
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import Data.String

-------------------------------------------------------------------------------
-- | Form to change the details of a single person.
--    We don't allow the userid to be edited because this is the primary
--    key for the person table.
formPerson
        :: [FeedForm]   -- ^ Feedback to add to current form.
        -> Path         -- ^ Path to submit form.
        -> Person       -- ^ Person details to populate form.
        -> Html

formPerson fsFeed path person
 = form ! A.action (H.toValue path)
 $ do
        -- Stash args from the target path as hidden fields.
        mapM_   (\(fieldName, fieldData)
                 -> input ! A.type_ "hidden"
                          ! A.name  (H.toValue fieldName)
                          ! A.value (H.toValue fieldData))
                (pathFields path)

        -- Feedback about updated and invalid fields.
        htmlFeedForm fsFeed (formTableNameOfEntity personEntity)

        -- Save button.
        H.br
        input   ! A.type_  "submit"
                ! A.class_ "buttonFull"
                ! A.value  "Save"

        -- Person details.
        divPersonDetails fsFeed person
        H.br

        -- Save button.
        input   ! A.type_  "submit"
                ! A.class_ "buttonFull"
                ! A.value  "Save"


-------------------------------------------------------------------------------
divPersonDetails :: [FeedForm] -> Person -> Html
divPersonDetails fsFeed person
 = H.div ! A.id "person-details-edit" ! A.class_ "details"
 $ do
        H.table $ trInputWithFocus fsFeed
                "FirstName"     "first name"
                (pretty $ personFirstName person)
                (Just "(required)")

        field   "PreferredName" "preferred name"
                (maybe "" pretty $ personPreferredName person)

        field   "FamilyName"    "family name"
                (maybe "" pretty $ personFamilyName person)

        field   "DateOfBirth"   "date of birth (dd-mm-yyyy)"
                (maybe "" pretty $ personDateOfBirth person)

        field   "MemberId"      "member id"
                (maybe "" pretty $ personMemberId person)

        field   "PhoneMobile"   "mobile phone number"
                (maybe "" pretty $ personPhoneMobile person)

        field   "PhoneFixed"    "fixed phone number"
                (maybe "" pretty $ personPhoneFixed person)

        field   "Email"         "email address"
                (maybe "" pretty $ personEmail person)

        -- TODO: add 'ok' feedback when updated.
        let sDojo   = fromString $ maybe "" pretty $ personDojoHome person
        H.table
         $ do   col ! A.class_ "DojoHome"
                tr $ th $ "home dojo"
                tr $ td $ (H.select ! A.name "DojoHome")
                        $ do    H.option ! A.value "" $ "(unspecified)"
                                forM_ ssDojos (optSelected sDojo)

        -- TODO: add 'ok' feedback when updated.
        let sMember = fromString $ maybe "" pretty $ personMembershipLevel person
        H.table
         $ do   col ! A.class_ "MembershipLevel"
                tr $ th $ "membership level"
                tr $ td $ (H.select ! A.name "MembershipLevel")
                        $ do    H.option ! A.value "" $ "(unspecified)"
                                forM_ ssMemberLevels (optSelected sMember)

        field   "MembershipRenewal" "membership renewal date"
                (maybe "" pretty $ personMembershipRenewal person)

        field   "EmergencyName1" "emergency contact name 1"
                (maybe "" pretty $ personEmergencyName1 person)

        field   "EmergencyPhone1" "emergency contact phone 1"
                (maybe "" pretty $ personEmergencyPhone1 person)

        field   "EmergencyName2" "emergency contact name 2"
                (maybe "" pretty $ personEmergencyName2 person)

        field   "EmergencyPhone2" "emergency contact phone 2"
                (maybe "" pretty $ personEmergencyPhone2 person)

 where
        field sClass sLabel sValue
         = H.table $ trInput_ fsFeed sClass sLabel sValue

        optSelected sSel sVal
         = (H.option
                !  A.value (fromString sVal)
                !? (sSel == sVal, A.selected, "true"))
                (H.toHtml sVal)

        -- TODO: get member types from database.
        ssMemberLevels
         = [ "Family Annual"
           , "Adult Annual"
           , "University Annual"
           , "Student Annual"
           , "Concession Annual"
           , "Child Annual"
           , "Family Intro (3 months)"
           , "Adult Intro (3 months)"
           , "University Intro (3 months)"
           , "Student Intro (3 months)"
           , "Concession Intro (3 months)"
           , "Child Intro (3 months)" ]

        -- TODO: get dojo list from database.
        ssDojos
         = [ "Armidale"
           , "Bega"
           , "Bellingen"
           , "Berala"
           , "Elands"
           , "Faulconbridge"
           , "Gordon"
           , "Granville"
           , "Hornsby"
           , "Katoomba"
           , "Leichhardt"
           , "Lismore"
           , "Roseville"
           , "Seven Hills"
           , "Sutherland" ]

