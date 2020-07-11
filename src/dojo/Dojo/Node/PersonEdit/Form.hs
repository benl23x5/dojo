
module Dojo.Node.PersonEdit.Form
        (formPerson)
where
import Dojo.Data.Person
import Dojo.Framework
import Dojo.Base
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


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
        htmlFeedForm fsFeed niceNameOfPersonField

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
                (pretty $ personPreferredName person)

        field   "FamilyName"    "family name"
                (pretty $ personFamilyName person)

        field   "DateOfBirth"   "date of birth (dd-mm-yyyy)"
                (pretty $ personDateOfBirth person)

        field   "MemberId"      "member id"
                (pretty $ personMemberId person)

        field   "PhoneMobile"   "mobile phone number"
                (pretty $ personPhoneMobile person)

        field   "PhoneFixed"    "fixed phone number"
                (pretty $ personPhoneFixed person)

        field   "Email"         "email address"
                (pretty $ personEmail person)

        field   "DojoHome"      "home dojo"
                (pretty $ personDojoHome person)

        field   "MembershipLevel" "membership level"
                (pretty $ personMembershipLevel person)

        field   "MembershipRenewal" "membership renewal date"
                (pretty $ personMembershipRenewal person)

        field   "EmergencyName1" "emergency contact name 1"
                (pretty $ personEmergencyName1 person)

        field   "EmergencyPhone1" "emergency contact phone 1"
                (pretty $ personEmergencyPhone1 person)

        field   "EmergencyName2" "emergency contact name 2"
                (pretty $ personEmergencyName2 person)

        field   "EmergencyPhone2" "emergency contact phone 2"
                (pretty $ personEmergencyPhone2 person)

 where
        field sClass sLabel sValue
         = H.table $ trInput_ fsFeed sClass sLabel sValue


