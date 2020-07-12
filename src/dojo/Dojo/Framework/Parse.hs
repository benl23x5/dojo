
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


-- Parse a day. We require year portion to be within the last hundred
-- years or so to ensure we don't write dates to the database that can't
-- be represented with the various Haskell epoch based date formats.
instance Parse Time.Day where
 parse str
  | (sDay,   '-' : str1) <- span isDigit str
  , (sMonth, '-' : str2) <- span isDigit str1
  , (sYear,  [])         <- span isDigit str2
  , length sDay   > 0
  , length sMonth > 0
  , length sYear  > 0
  = check (read sDay) (read sMonth) (read sYear)

  | (sDay,   ' ' : str1) <- span isDigit str
  , (sMonth, ' ' : str2) <- span isAlpha str1
  , (sYear, [])          <- span isDigit str2
  , length sDay > 0
  , Just nMonth <- lookup sMonth
                $  zip [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
                       , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"] [1..]
  , length sYear > 0
  = check (read sDay) nMonth (read sYear)

  | otherwise
  = Left  (ParseError "date must have format dd-mm-yyyy")

  where check nDay nMonth nYear
         | not $ nYear >= 1900
         = Left (ParseError "date must be after 1900")

         | not $ nYear <  2100
         = Left (ParseError "date must be before 2100")

         | otherwise
         = case Time.fromGregorianValid nYear nMonth nDay of
            Just date -> Right date
            Nothing   -> Left (ParseError "date must be valid in Gregorian calendar")



instance Parse Time.TimeOfDay where
 parse str
  | (sHour, ':' : str1) <- span isDigit str
  , (sMin,  [])         <- span isDigit str1
  , length sHour > 0
  , length sMin  > 0
  = check (read sHour) (read sMin)

  | otherwise
  = Left  (ParseError "time must have format hh:mm")

  where check nHour nMin
         = case Time.makeTimeOfDayValid nHour nMin (fromIntegral (0 :: Int)) of
                Just tod -> Right tod
                Nothing  -> Left (ParseError "time must be valid in 24hr range")

