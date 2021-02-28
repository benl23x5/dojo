{-# LANGUAGE UndecidableInstances #-}
module Dojo.Framework.Pretty where
import Dojo.Base
import qualified Data.Time              as Time
import qualified Text.Blaze.Html5       as H


class Pretty a where
 pretty :: a -> String

instance {-# INCOHERENT #-} Pretty a => H.ToMarkup a where
 toMarkup = H.toMarkup . pretty


instance Pretty Integer where
 pretty = show

instance Pretty Int where
 pretty = show


instance Pretty String where
 pretty = id


instance Pretty Time.Day where
 pretty day
  = let (year, month, weekday)      = Time.toGregorian day
    in             (padRc '0' 2 $ show $ weekday)
        ++ "-" ++  (padRc '0' 2 $ show $ month)
        ++ "-" ++  (padRc '0' 2 $ show $ year)


instance Pretty Time.TimeOfDay where
 pretty tod
        =          (padRc '0' 2 $ show $ Time.todHour tod)
        ++ ":" ++  (padRc '0' 2 $ show $ Time.todMin  tod)

padRc :: Char -> Int -> String -> String
padRc c len str
        = replicate (len - length str) c ++ str
