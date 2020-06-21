
module Dojo.Framework.Form
        ( trInput
        , trInputWithFocus
        , thInputFeedback
        , tdInputFeedback)
where
import Dojo.Base
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

type ReplyUpdated = [String]
type ReplyInvalid = [(String, String)]


-- | Construct a table for a single input field.
trInput :: ReplyUpdated -> ReplyInvalid
        -> String -> String -> String -> Html

trInput rsUpdate rsInvalid sClassName sDisplayLabel sValue
 = do   tr $ thInputFeedback rsUpdate rsInvalid sClassName sDisplayLabel
        tr $ tdInputFeedback False rsInvalid sClassName sValue


-- | Construct a table for a single input field.
trInputWithFocus
        :: ReplyUpdated -> ReplyInvalid
        -> String -> String -> String -> Html

trInputWithFocus rsUpdate rsInvalid sClassName sDisplayLabel sValue
 = do   tr $ thInputFeedback rsUpdate rsInvalid sClassName sDisplayLabel
        tr $ tdInputFeedback True rsInvalid sClassName sValue


-- | Column header in event details.
thInputFeedback :: [String] -> [(String, String)] -> String -> String -> Html
thInputFeedback fieldsUpdated fieldValsInvalid fieldName niceName

 -- Feedback entry field was just updated.
 | elem fieldName fieldsUpdated
 = th   ! A.class_ (H.toValue ("updated "  ++ fieldName))
        $ (H.toMarkup $ niceName ++ " (ok)")

 -- Feedback entry field contains invalid value.
 | isJust $ lookup fieldName fieldValsInvalid
 = th   ! A.class_ (H.toValue ("invalid " ++ fieldName))
        $ (H.toMarkup $ niceName ++ " (invalid)")

 -- Regular column header.
 | otherwise
 = th   ! A.class_ (H.toValue fieldName)
        $ (H.toMarkup $ niceName)



-- | Field of event details.
--      If the field contains invalid data then continue displaying
--      this date, but focus on the field.
tdInputFeedback
        :: ToValue a
        => Bool
        -> [(String, String)]
        -> String
        -> a
        -> Html

tdInputFeedback takeFocus fieldValsInvalid fieldName val

 -- Feedback entry field contains invalid value.
 | Just badVal <- lookup fieldName fieldValsInvalid
 = td   $ input !  A.name   (H.toValue fieldName)
                !  A.autocomplete "off"
                !  A.value  (H.toValue badVal)
                !? (takeFocus, A.autofocus, "on")

 -- Just show the entry field.
 | otherwise
 = td   $ input ! A.name   (H.toValue fieldName)
                ! A.autocomplete "off"
                ! A.value  (H.toValue val)
                !? (takeFocus, A.autofocus, "on")

