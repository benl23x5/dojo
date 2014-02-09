
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
formPerson :: [Arg] -> Path -> Person -> Html
formPerson args path person
 = form ! A.action (H.toValue path)
 $ do   
        mapM_   (\(fieldName, fieldData)
                 -> input ! A.type_ "hidden"
                          ! A.name  (H.toValue fieldName)
                          ! A.value (H.toValue fieldData))
                (pathFields path)

        divPersonDetails args person

        br

        -- Save button, 
        --  with feedback on which fields were updated next to it.
        table
         $ tr $ td 
         $ do   input   ! A.type_ "submit" 
                        ! A.value "Save"

                let updatedFields    = [field | ArgDetailsUpdated field <- args]
                when (not $ null updatedFields)
                 $ H.span ! A.class_ "updated"
                 $ H.toMarkup
                 $ " Updated: " 
                        ++ intercalate ", " 
                                 ( map (\(Just n) -> n) 
                                 $ map niceNameOfPersonField updatedFields)
                        ++ "."


divPersonDetails :: [Arg] -> Person -> Html
divPersonDetails args person
 = H.div ! A.id "person-details-edit"
 $ H.table
 $ do   H.table
         $ do   tr $ do th' "PreferedName"   "prefered name"
                        th' "FirstName"      "first name"
                        th' "MiddleName"     "middle name"
                        th' "FamilyName"     "family name"
                        th' "DateOfBirth"    "date of birth"

                tr $ do tdF "PreferedName"   (pretty $ personPreferedName person)
                        td' "FirstName"      (pretty $ personFirstName    person)
                        td' "MiddleName"     (pretty $ personMiddleName   person)
                        td' "FamilyName"     (pretty $ personFamilyName   person)
                        td' "DateOfBirth"    (pretty $ personDateOfBirth  person)

        br
        H.table
         $ do   tr $ do th' "MemberId"       "member id"
                        th' "Mobile"         "mobile number"
                        th' "Email"          "email address"

                tr $ do td' "MemberId"       (pretty $ personMemberId person)
                        td' "Mobile"         (pretty $ personMobile   person)
                        td' "Email"          (pretty $ personEmail    person)

 where  -- Feedback which fields were just updated.
        updateds = mapMaybe takeDetailsUpdated args

        -- Feedback which fields have invalid values.
        invalids = mapMaybe takeDetailsInvalid args

        th' fieldName niceName
         = thInputFeedback updateds invalids fieldName niceName

        tdF name val = tdInput True  name val
        td' name val = tdInput False name val

        tdInput focus name val
         = tdInputFeedback focus invalids name val

