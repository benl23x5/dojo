
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


-------------------------------------------------------------------------------
-- | Show the event details summary, which is not editable.
divEventShowDetails :: EventDetails -> H.Html
divEventShowDetails details
 = H.div ! A.id "event-details-edit" ! A.class_ "details"
 $ do
        let event       = eventDetailsEvent details
        let mpCreated   = eventDetailsCreatedByPerson details
        let muCreated   = eventDetailsCreatedByUser details

        H.table $ do
         tr $ td $ H.string
            $ maybe "[sometype]" (\v -> pretty v ++ " class") (eventType event)
            ++ " by "
            ++ (fromMaybe "[someperson]"
                (join $ fmap personDisplayName mpCreated))
            ++ " ("
            ++ (maybe "[someuser]" (pretty . userName) muCreated)
            ++ ")."

         tr $ td $ H.string
            $  maybe "[somewhere]" pretty (eventLocation event)
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

        -- When this is a new event put focus on the location input field,
        -- otherwise allow focus to be taken by the last person entry field.
        let sDojo = maybe "" pretty $ eventLocation event
        H.table
         $ do   tr $ do th "location"
                tr $ do td $ (H.select ! A.name "Location")
                         $ do   H.option ! A.value "" $ "(unspecified)"
                                forM_ (map pretty dojos) (optSelected sDojo)

        let sType = maybe "" pretty $ eventType event
        H.table
         $ do   tr $ do th "type"
                tr $ do td $ (H.select ! A.name "Type")
                         $ do   H.option ! A.value "" $ "(unspecified)"
                                forM_ (map pretty eventTypes) (optSelected sType)

 where  optSelected sSel sVal
         = (H.option
                !  A.value (fromString sVal)
                !? (sSel == sVal, A.selected, "true"))
                (H.toHtml sVal)

