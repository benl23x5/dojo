
module Dojo.Node.EventEdit.Details where
import Dojo.Data.Event
import Dojo.Data.User
import Dojo.Data.Person
import Dojo.Framework.Form
import Dojo.Framework
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import Data.String

-------------------------------------------------------------------------------
-- | Identifying details of an event.
data EventDetails
        = EventDetails
        { eventDetailsEvent             :: Event
        , eventDetailsCreatedByUser     :: Maybe User
        , eventDetailsCreatedByPerson   :: Maybe Person
        , eventDetailsEventTypes        :: [EventType]
        , eventDetailsDojosAvail        :: [PersonDojo] }
        deriving Show


-- | Event details.
divEventDescription :: Event -> Maybe User -> Person -> Html
divEventDescription event mUserCreatedBy personCreatedBy
 = H.div ! A.class_ "event-description"
 $ H.table
 $ do   H.tr $ H.td $ H.string
           $  maybe "[event]" eventTypeClassName (eventType event)
           ++ " by "
           ++ maybe "" pretty (personDisplayName personCreatedBy)
           ++ (case mUserCreatedBy of
                Nothing -> ""
                Just userCreatedBy
                 -> " (" ++ pretty (userName userCreatedBy) ++ ")")
           ++ "."

        H.tr $ H.td $ H.string
           $  maybe "[somewhere]" pretty  (eventLocation event)
           ++ maybe "[someday]"  (\v -> " on " ++ pretty v) (eventDate event)
           ++ maybe "[sometime]" (\v -> " at " ++ pretty v) (eventTime event)
           ++ "."


-------------------------------------------------------------------------------
-- | Show the form to edit event details.
divEventEditDetails :: EventDetails -> [FeedForm] -> H.Html
divEventEditDetails details fsForm
 = H.div ! A.id "event-details-edit" ! A.class_ "details"
 $ do
        let event       = eventDetailsEvent details
        let eventTypes  = eventDetailsEventTypes details
        let dojos       = eventDetailsDojosAvail details

        let sType = maybe "" pretty $ eventType event
        H.table
         $ do   H.tr $ do thInputFeedback fsForm "Type" "type"
                H.tr $ do H.td $ (H.select ! A.name "Type")
                           $ do H.option ! A.value "" $ "(unspecified)"
                                forM_ (map pretty eventTypes) (optSelected sType)

        -- When this is a new event put focus on the location input field,
        -- otherwise allow focus to be taken by the last person entry field.
        let sDojo = maybe "" pretty $ eventLocation event
        H.table
         $ do   H.tr $ do thInputFeedback fsForm "Location" "location"
                H.tr $ do H.td $ (H.select ! A.name "Location")
                           $ do H.option ! A.value "" $ "(unspecified)"
                                forM_ (map pretty dojos) (optSelected sDojo)

        tableFields fsForm
         [ ( "Date", "date (dd-mm-yyyy)"
           , maybe "" pretty $ eventDate event
           , Just "(required)"
           , False) ]

        tableFields fsForm
         [ ( "Time", "time (hh:mm 24hr)"
           , maybe "" pretty $ eventTime event
           , Just "(required)"
           , False)
         ]

 where  optSelected sSel sVal
         = (H.option
                !  A.value (fromString sVal)
                !? (sSel == sVal, A.selected, "true"))
                (H.toHtml sVal)

