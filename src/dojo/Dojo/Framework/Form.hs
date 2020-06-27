
-- | Form framework.
--
--   Helpers for producing forms that include feedback on each field.
--   We can attach feedback saying that a particular field either has
--   valid data and has just been updated, or has invalid data that
--   cannot be parsed or is out of range for the given field.
--
module Dojo.Framework.Form
        ( FeedForm(..)
        , trInput
        , trInputWithFocus, trInputWithFocus_
        , thInputFeedback
        , tdInputFeedback
        , htmlFeedForm)
where
import Dojo.Base
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | Feedback to add to a displayed form.
data FeedForm
        -- ^ Feedback that a field has been updated.
        = FeedFormUpdated
        { feedField     :: String }     -- ^ Name of the field.

        -- ^ Feedback that a field contains invalid data.
        | FeedFormInvalid
        { feedField     :: String       -- ^ Name of the field.
        , feedValue     :: String       -- ^ Current contents of field.
        , feedError     :: String       -- ^ Description of error.
        }
        deriving Show

type FieldClass = String
type FieldLabel = String
type FieldValue = String


-------------------------------------------------------------------------------
-- | Construct a table for a single input field.
trInput :: [FeedForm] -> FieldClass -> FieldLabel -> FieldValue -> Html
trInput fsFeed sClassName sDisplayLabel sValue
 = do   tr $ thInputFeedback fsFeed sClassName sDisplayLabel
        tr $ tdInputFeedback False fsFeed sClassName sValue Nothing


-- | Construct a table for a single input field.
trInputWithFocus_
        :: [FeedForm] -> FieldClass -> FieldLabel -> FieldValue -> Html
trInputWithFocus_ fsFeed sClassName sDisplayLabel sValue
 = do   trInputWithFocus fsFeed sClassName sDisplayLabel sValue Nothing


trInputWithFocus
        :: [FeedForm]
        -> FieldClass -> FieldLabel -> FieldValue
        -> Maybe String
        -> Html

trInputWithFocus fsFeed sClassName sDisplayLabel sValue mPlaceholder
 = do   tr $ thInputFeedback fsFeed sClassName sDisplayLabel
        tr $ tdInputFeedback True fsFeed sClassName sValue mPlaceholder


-- | Column header in field details.
thInputFeedback
        :: [FeedForm] -> FieldClass -> FieldLabel -> Html
thInputFeedback fsFeed fieldName niceName

 -- Feedback entry field contains invalid value.
 | Just sError <- takeHead
        [sError | FeedFormInvalid f _ sError <- fsFeed
                , f == fieldName]
 = th   ! A.class_ (H.toValue ("invalid " ++ fieldName))
        $ (H.toMarkup $ niceName ++ " (" ++ sError ++ ")")

 -- Feedback entry field was just updated.
 | elem fieldName [ f | FeedFormUpdated f <- fsFeed]
 = th   ! A.class_ (H.toValue ("updated "  ++ fieldName))
        $ (H.toMarkup $ niceName ++ " (ok)")

 -- Regular column header.
 | otherwise
 = th   ! A.class_ (H.toValue fieldName)
        $ (H.toMarkup $ niceName)


-- | Field of event details.
--    If the field contains invalid contents then continue displaying
--    the contents and take the focus.
tdInputFeedback
        :: ToValue a
        => Bool         -- ^ Whether to take focus
        -> [FeedForm]   -- ^ Feedback to add to the field/
        -> FieldClass
        -> a            -- ^ Field value.
        -> Maybe String -- ^ Placeholder text.
        -> Html

tdInputFeedback bHintFocus fsFeed fieldName val mPlaceholder

 -- Feedback entry field contains invalid value.
 | Just sValue  <- takeHead
        [ sValue | FeedFormInvalid sName sValue _ <- fsFeed
                 , sName == fieldName]
 = td   $ input !  A.name   (H.toValue fieldName)
                !  A.autocomplete "off"
                !  A.autofocus "on"
                !  A.value  (H.toValue sValue)

 | otherwise
 = do   let bOtherErrors
             = not $ null
             $ [sName | FeedFormInvalid sName _ _ <- fsFeed]

        let bTakeFocus
             = bHintFocus && not bOtherErrors

        case mPlaceholder of
         Nothing
          -> td $ input
                ! A.name   (H.toValue fieldName)
                ! A.autocomplete "off"
                ! A.value  (H.toValue val)
                !? (bTakeFocus, A.autofocus, "on")

         Just sPlaceholder
          -> td $ input
                ! A.name   (H.toValue fieldName)
                ! A.autocomplete "off"
                ! A.value  (H.toValue val)
                ! A.placeholder (H.toValue sPlaceholder)
                !? (bTakeFocus, A.autofocus, "on")


-------------------------------------------------------------------------------
htmlFeedForm :: [FeedForm] -> (String -> Maybe String) -> Html
htmlFeedForm fsFeed fNiceName
 = do   let fsInvalid = [sField | FeedFormInvalid sField _ _sErr <- fsFeed]
        let fsUpdated = [sField | FeedFormUpdated sField <- fsFeed]

        when (not $ null fsInvalid)
         $ do   H.br
                H.span ! A.class_ "invalid"
                 $ H.toMarkup
                 $ " Invalid: "
                        ++ (intercalate ", "
                            [ fromMaybe s $ fNiceName s
                            | s <- fsInvalid ])
                       ++ "."
                H.br

        when (not $ null fsUpdated)
         $ do   H.br
                H.span ! A.class_ "updated"
                 $ H.toMarkup
                 $ " Updated: "
                        ++ (intercalate ", "
                            [ fromMaybe s $ fNiceName s
                            | s <- fsUpdated ])
                       ++ "."
                H.br
