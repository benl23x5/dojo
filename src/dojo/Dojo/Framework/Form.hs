
-- | Form framework.
--
--   Helpers for producing forms that include feedback on each field.
--   We can attach feedback saying that a particular field either has
--   valid data and has just been updated, or has invalid data that
--   cannot be parsed or is out of range for the given field.
--
module Dojo.Framework.Form
        ( FeedForm(..)
        , FieldClass, FieldLabel, FieldValue, FieldHolder
        , tableFields
        , trInput, trInput_
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
        deriving (Eq, Show)

type FieldClass  = String
type FieldLabel  = String
type FieldValue  = String
type FieldHolder = String


-------------------------------------------------------------------------------
-- | Produce a table with the given input fields in a single row.
tableFields
 :: [FeedForm]
 -> [(FieldClass, FieldLabel, FieldValue, Maybe FieldHolder, Bool)]
 -> Html

tableFields fsFeed fs
 = H.table $ do
        H.tr $ forM_ fs $ \(sClass, sLabel, _sValue, _mHolder, _bFocus) -> do
                thInputFeedback fsFeed sClass sLabel

        H.tr $ forM_ fs $ \(sClass, _sLabel, sValue, mHolder, bFocus) -> do
                tdInputFeedback bFocus fsFeed sClass sValue mHolder


-------------------------------------------------------------------------------
-- | A table for a single input field,
--   with optional placholder text.
trInput :: [FeedForm]
        -> FieldClass -> FieldLabel -> FieldValue
        -> Maybe FieldHolder
        -> Html

trInput fsFeed sClassName sDisplayLabel sValue mPlaceholder
 = do   H.tr $ thInputFeedback fsFeed sClassName sDisplayLabel
        H.tr $ tdInputFeedback False fsFeed sClassName sValue mPlaceholder


-- | A table for a single input field,
--   without placeholder text.
trInput_ :: [FeedForm] -> FieldClass -> FieldLabel -> FieldValue -> Html
trInput_ fsFeed sClassName sDisplayLabel sValue
 = do   trInput fsFeed sClassName sDisplayLabel sValue Nothing


-- | A focused table row for a single input field,
--   with optional placeholder text.
trInputWithFocus
        :: [FeedForm]
        -> FieldClass -> FieldLabel -> FieldValue
        -> Maybe FieldHolder
        -> Html

trInputWithFocus fsFeed sClassName sDisplayLabel sValue mPlaceholder
 = do   H.tr $ thInputFeedback fsFeed sClassName sDisplayLabel
        H.tr $ tdInputFeedback True fsFeed sClassName sValue mPlaceholder


-- | A table row for a single input field,
--   without placeholder text.
trInputWithFocus_
        :: [FeedForm] -> FieldClass -> FieldLabel -> FieldValue -> Html
trInputWithFocus_ fsFeed sClassName sDisplayLabel sValue
 = do   trInputWithFocus fsFeed sClassName sDisplayLabel sValue Nothing


-------------------------------------------------------------------------------
-- | Column header in field details.
thInputFeedback
        :: [FeedForm] -> FieldClass -> FieldLabel -> Html
thInputFeedback fsFeed fieldName niceName

 -- Feedback entry field contains invalid value.
 | Just sError <- takeHead
        [sError | FeedFormInvalid f _ sError <- fsFeed
                , f == fieldName]
 = H.th ! A.class_ (H.toValue ("invalid " ++ fieldName))
        $ (H.toMarkup $ niceName ++ " (" ++ sError ++ ")")

 -- Feedback entry field was just updated.
 | elem fieldName [ f | FeedFormUpdated f <- fsFeed]
 = H.th ! A.class_ (H.toValue ("updated "  ++ fieldName))
        $ (H.toMarkup $ niceName ++ " (ok)")

 -- Regular column header.
 | otherwise
 = H.th ! A.class_ (H.toValue fieldName)
        $ (H.toMarkup $ niceName)


-- | Field of event details.
--    If the field contains invalid contents then continue displaying
--    the contents and take the focus.
tdInputFeedback
        :: H.ToValue a
        => Bool                 -- ^ Whether to take focus
        -> [FeedForm]           -- ^ Feedback to add to the field/
        -> FieldClass
        -> a                    -- ^ Field value.
        -> Maybe FieldHolder    -- ^ Placeholder text.
        -> Html

tdInputFeedback bHintFocus fsFeed fieldName val mPlaceholder

 -- Feedback entry field contains invalid value.
 | Just sValue  <- takeHead
        [ sValue | FeedFormInvalid sName sValue _ <- fsFeed
                 , sName == fieldName]
 = H.td $ H.input
        ! A.name   (H.toValue fieldName)
        ! A.autocomplete "off"
        ! A.autofocus "on"
        ! A.value  (H.toValue sValue)

 | otherwise
 = do   -- Whether any of the other fields had errors.
        let bOtherErrors
             = not $ null
             $ [sName | FeedFormInvalid sName _ _ <- fsFeed]

        -- Only take focus if there were no errors in other fields.
        -- If there was an error in another field we want to focus
        -- on that one instead.
        let bTakeFocus
             = bHintFocus && not bOtherErrors

        case mPlaceholder of
         Nothing
          -> H.td $ H.input
                ! A.name   (H.toValue fieldName)
                ! A.autocomplete "off"
                ! A.value  (H.toValue val)
                !? (bTakeFocus, A.autofocus, "on")

         Just sPlaceholder
          -> H.td $ H.input
                ! A.name   (H.toValue fieldName)
                ! A.autocomplete "off"
                ! A.value  (H.toValue val)
                ! A.placeholder (H.toValue sPlaceholder)
                !? (bTakeFocus, A.autofocus, "on")


-------------------------------------------------------------------------------
htmlFeedForm :: [FeedForm] -> (String -> Maybe String) -> Html
htmlFeedForm fsFeed fNiceName
 = do   let fsInvalid = [sField | FeedFormInvalid sField _ _sErr <- fsFeed]
--        let fsUpdated = [sField | FeedFormUpdated sField <- fsFeed]

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
{-
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
-}