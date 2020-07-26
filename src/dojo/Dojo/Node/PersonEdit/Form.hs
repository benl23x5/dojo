
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
        -> [PersonDojo] -- ^ Current dojos list.
        -> [PersonMembershipLevel]
        -> Html

formPerson fsFeed path person dojos memberLevels
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
        divPersonDetails fsFeed person dojos memberLevels
        H.br

        -- Save button.
        input   ! A.type_  "submit"
                ! A.class_ "buttonFull"
                ! A.value  "Save"


-------------------------------------------------------------------------------
divPersonDetails
        :: [FeedForm]
        -> Person -> [PersonDojo] -> [PersonMembershipLevel]
        -> Html

divPersonDetails fsFeed person dojos memberLevels
 = H.div ! A.id "person-details-edit" ! A.class_ "details"
 $ do
        -- If the first name is not filled in then the form
        --  will not accept the update.
        H.table $ trInputWithFocus fsFeed
                "FirstName"     "first name"
                (maybe "" pretty $ personFirstName person)
                (Just "(required)")

        fieldm  "PreferredName" "preferred name"
                (personPreferredName person)

        fieldm  "FamilyName"    "family name"
                (personFamilyName person)

        fieldm  "DateOfBirth"   "date of birth (dd-mm-yyyy)"
                (personDateOfBirth person)

        fieldm  "MemberId"      "member id"
                (personMemberId person)

        fieldm  "PhoneMobile"   "mobile phone number"
                (personPhoneMobile person)

        fieldm  "PhoneFixed"    "fixed phone number"
                (personPhoneFixed person)

        fieldm  "Email"         "email address"
                (personEmail person)

        -- TODO: add 'ok' feedback when updated.
        let sDojo   = fromString $ maybe "" pretty $ personDojoHome person
        H.table
         $ do   col ! A.class_ "DojoHome"
                tr $ th $ "home dojo"
                tr $ td $ (H.select ! A.name "DojoHome")
                        $ do    H.option ! A.value "" $ "(unspecified)"
                                forM_ (map pretty dojos) (optSelected sDojo)

        -- TODO: add 'ok' feedback when updated.
        let sMember = fromString $ maybe "" pretty $ personMembershipLevel person
        H.table
         $ do   col ! A.class_ "MembershipLevel"
                tr $ th $ "membership level"
                tr $ td $ (H.select ! A.name "MembershipLevel")
                        $ do    H.option ! A.value "" $ "(unspecified)"
                                forM_ (map pretty memberLevels) (optSelected sMember)

        fieldm  "MembershipRenewal" "membership renewal date"
                (personMembershipRenewal person)

        fieldm  "EmergencyName1" "emergency contact name 1"
                (personEmergencyName1 person)

        fieldm  "EmergencyPhone1" "emergency contact phone 1"
                (personEmergencyPhone1 person)

        fieldm  "EmergencyName2" "emergency contact name 2"
                (personEmergencyName2 person)

        fieldm  "EmergencyPhone2" "emergency contact phone 2"
                (personEmergencyPhone2 person)

 where
        fieldm sClass sLabel xValue
         = H.table $ trInput_ fsFeed sClass sLabel
                   $ maybe "" pretty xValue

        optSelected sSel sVal
         = (H.option
                !  A.value (fromString sVal)
                !? (sSel == sVal, A.selected, "true"))
                (H.toHtml sVal)
