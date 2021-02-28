
module Dojo.Framework.Lens
        ( RowLens       (..)
        , ColLens       (..)
        , colOfRow
        , trOfRowLens

        , trFormRowOfRowLens
        , trFormRowOfColLens

        , trViewRowOfRowLens
        , trViewRowOfColLens)
where
import Dojo.Base
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


data RowLens a
        = RowLens [ColLens a]

data ColLens a
        = ColLens
        { colLensName           :: String
        , colLensNiceName       :: String
        , colLensFormType       :: String
        , colLensSqlType        :: String
        , colLensShow           :: a -> String
        , colLensSqlValue       :: a -> SqlValue }


colOfRow :: RowLens a -> String -> Maybe (ColLens a)
colOfRow (RowLens cols) fieldName
 = find (\c -> colLensName c == fieldName) cols


trOfRowLens :: a -> RowLens a -> Html
trOfRowLens val (RowLens cols)
 = H.tr
 $ do   mapM_ (\lens -> H.td $ H.toMarkup $ colLensShow lens val) cols


-- | Build a form input field for the named column in this row.
trFormRowOfRowLens :: a -> RowLens a -> String -> Html
trFormRowOfRowLens val (RowLens cols) fieldName
 = let  Just column = find (\c -> colLensName c == fieldName) cols
   in   trFormRowOfColLens val column


-- | Build a form input field for the given column.
trFormRowOfColLens :: a -> ColLens a -> Html
trFormRowOfColLens val colLens
 = H.tr
 $ do   H.th (H.toMarkup $ colLensNiceName colLens)
        H.td $ H.input
         ! A.type_ (H.toValue $ colLensFormType colLens)
         ! A.name  (H.toValue $ colLensName     colLens)
         ! A.autocomplete "off"
         ! A.value (H.toValue $ colLensShow     colLens val)


-- View -----------------------------------------------------------------------
-- | Build a form input field for the named column in this row.
trViewRowOfRowLens :: a -> RowLens a -> String -> Html
trViewRowOfRowLens val (RowLens cols) fieldName
 = let  Just column     = find (\c -> colLensName c == fieldName) cols
   in   trViewRowOfColLens val column


-- | Build a form input field for the given column.
trViewRowOfColLens :: a -> ColLens a -> Html
trViewRowOfColLens val colLens
 = H.tr
 $ do   H.th (H.toMarkup $ colLensNiceName colLens)
        H.td (H.toMarkup $ colLensShow     colLens val)

{-              ! A.type_ (H.toValue $ colLensFormType colLens)
                ! A.name  (H.toValue $ colLensName     colLens)
                ! A.autocomplete "off"
                ! A.value ()
-}
