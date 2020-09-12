
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
 = form ! A.action (H.toValue $ eventFormPath eform)
 $ do
        let path        = eventFormPath eform
        let fsForm      = eventFormFeedForm eform

        -- Stash args from the target path as hidden fields.
        mapM_   (\(fieldName, fieldData)
                 -> input ! A.type_ "hidden"
                          ! A.name  (H.toValue fieldName)
                          ! A.value (H.toValue fieldData))
                (pathFields path)

        -- Feedback about updated and invalid fields.
        htmlFeedForm fsForm niceNameOfEventField

        let details
                = EventDetails
                { eventDetailsEvent             = eventFormEventValue eform
                , eventDetailsCreatedByUser     = eventFormCreatedByUser eform
                , eventDetailsCreatedByPerson   = eventFormCreatedByPerson eform
                , eventDetailsEventTypes        = eventFormEventTypes eform
                , eventDetailsDojosAvail        = eventFormDojosAvail eform }

        let event = eventFormEventValue eform
        divEventShowDetails details
         $ case eventId event of
            Nothing     -> []
            Just eid    -> [tdPath $ pathEventView ss eid]

        divEventEditDetails details fsForm
        H.br

        if (eventFormDetailsEditable eform)
         then input ! A.type_  "submit"
                    ! A.class_ "button-full"
                    ! A.value  "Save"

         else input ! A.type_  "submit"
                    ! A.class_ "input-hidden"
                    ! A.value  "Save"
