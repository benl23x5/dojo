
module Dojo.Node.PersonEdit.Form
        (formPerson)
where
import Dojo.Node.PersonEdit.Arg
import Dojo.Data.Person
import Dojo.Framework
import Dojo.Base
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | Form to change the details of a single person.
--      We don't allow the userid to be edited because this is the primary
--      key for the person table.
formPerson
        :: [Arg]
        -> Path
        -> Person -> Html
formPerson args path person
 = form ! A.action (H.toValue path)
 $ do
        mapM_   (\(fieldName, fieldData)
                 -> input ! A.type_ "hidden"
                          ! A.name  (H.toValue fieldName)
                          ! A.value (H.toValue fieldData))
                (pathFields path)

        -- Feedback about which fields have been updated.
        let updatedFields = [field | ArgDetailsUpdated field <- args]
        when (not $ null updatedFields)
         $ do   H.br
                H.span ! A.class_ "updated"
                 $ H.toMarkup
                 $ " Updated: "
                       ++ (intercalate ", "
                            [ fromMaybe s $ niceNameOfPersonField s
                            | s <- updatedFields ])
                       ++ "."
                H.br

        -- Person details.
        divPersonDetails args person
        H.br

        -- Save button.
        input   ! A.type_  "submit"
                ! A.class_ "buttonFull"
                ! A.value  "Save"


divPersonDetails :: [Arg] -> Person -> Html
divPersonDetails args person
 = H.div ! A.id "person-details-edit" ! A.class_ "details"
 $ do
        fieldWithFocus
                "FirstName"     "first name (required)"
                (pretty $ personFirstName person)

        field   "PreferredName" "preferred name"
                (pretty $ personPreferredName person)

        field   "FamilyName"    "family name"
                (pretty $ personFamilyName person)

        field   "DateOfBirth"   "date of birth (dd-mm-yyyy)"
                (pretty $ personDateOfBirth person)

        field   "MemberId"      "member id"
                (pretty $ personMemberId person)

        field   "Mobile"        "mobile number"
                (pretty $ personMobile person)

        field   "Email"         "email address"
                (pretty $ personEmail person)

 where
        -- Fedeback about updated and invalid fields.
        fsFeed = mapMaybe takeFeedForm args

        fieldWithFocus sClass sLabel sValue
         = H.table $ trInputWithFocus fsFeed sClass sLabel sValue

        field sClass sLabel sValue
         = H.table $ trInput fsFeed sClass sLabel sValue


