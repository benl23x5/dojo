
module Dojo.Node.EventEdit.FormDetails (formEventDetails) where
import Dojo.Data.Session
import Dojo.Node.EventEdit.Base
import Dojo.Node.EventEdit.Details
import Dojo.Data.Event
import Dojo.Framework.Form
import Dojo.Framework
import Dojo.Paths
import Dojo.Chrome

import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | Produce a html form to edit details of a single event.
formEventDetails :: Session -> EventForm -> Html
formEventDetails ss eform
 = H.form ! A.action (H.toValue $ eventFormPath eform)
 $ do
        let path        = eventFormPath eform
        let fsForm      = eventFormFeedForm eform

        -- Stash args from the target path as hidden fields.
        mapM_   (\(fieldName, fieldData)
                 -> H.input ! A.type_ "hidden"
                            ! A.name  (H.toValue fieldName)
                            ! A.value (H.toValue fieldData))
                (pathFields path)

        let event = eventFormEventValue eform
        let details
                = EventDetails
                { eventDetailsEvent             = eventFormEventValue eform
                , eventDetailsCreatedByUser     = eventFormCreatedByUser eform
                , eventDetailsCreatedByPerson   = eventFormCreatedByPerson eform
                , eventDetailsEventTypes        = eventFormEventTypes eform
                , eventDetailsDojosAvail        = eventFormDojosAvail eform }

        -- Only bother showing site user name to admin users.
        let mUserCreatedBy
                = if sessionIsAdmin ss
                        then eventFormCreatedByUser eform
                        else Nothing

        let Just personCreatedBy
                = eventFormCreatedByPerson eform

        divEventDescription event
                mUserCreatedBy
                personCreatedBy

        (case eventId event of
          Nothing  -> return ()
          Just eid -> tableActions [pathEventView ss eid])

        divEventEditDetails details fsForm

        (if (eventFormDetailsEditable eform)
         then H.input ! A.type_  "submit"
                      ! A.class_ "button-full"
                      ! A.value  "Save"

         else H.input ! A.type_  "submit"
                      ! A.class_ "input-hidden"
                      ! A.value  "Save")

        -- Feedback about updated and invalid fields.
        htmlFeedForm fsForm niceNameOfEventField
