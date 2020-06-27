
module Dojo.Framework.Parse
        ( Parse         (..)
        , ParseError    (..))
where
import Dojo.Base
import qualified Data.Time      as Time


class Parse a where
 parse :: String -> Either ParseError a

data ParseError
        = ParseError String


-------------------------------------------------------------------------------
instance Parse Integer where
 parse str
  | all isDigit str
  , not $ null str
  = Right (read str)

  | otherwise
  = Left  (ParseError "must be an integer")


-- Parse a day. We require year portion to be within the last hundred years
-- or so to ensure we don't write dates to the database that can't be represented
-- with the various Haskell epoch based date formats.
instance Parse Time.Day where
 parse str
  | (nDay,   '-' : str1) <- span isDigit str
  , (nMonth, '-' : str2) <- span isDigit str1
  , (nYear,  [])         <- span isDigit str2
  , length nDay   > 0
  , length nMonth > 0
  , length nYear  > 0
  , year                 <- read nYear
  , Just day             <- Time.fromGregorianValid
                                year (read nMonth) (read nDay)
  , year >= 1900
  , year <  2100
  = Right day

  | otherwise
  = Left  (ParseError "must be a date with format DD-MM-YYYY")


instance Parse Time.TimeOfDay where
 parse str
  | (nHour, ':' : str1) <- span isDigit str
  , (nMin,  [])         <- span isDigit str1
  , length nHour > 0
  , length nMin  > 0
  , Just tod            <- Time.makeTimeOfDayValid
                                (read nHour) (read nMin) (fromIntegral (0 :: Int))
  = Right tod

  | otherwise
  = Left  (ParseError "must be a time with format HH:MM")
