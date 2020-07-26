
module Dojo.Framework.Parse
        ( Parse         (..)
        , ParseError    (..)
        , chompInput
        , loadInput)
where
import Dojo.Base
import qualified Data.Time      as Time
import qualified Data.Char      as Char

class Parse a where
 parse :: String -> Either ParseError a

data ParseError
        = ParseError String
        deriving Show


-------------------------------------------------------------------------------
-- | Check an input string contains only printable characters,
--   chomp pre and post whitespace from the ends,
--   hard limit input to 4k chars,
--   and replace all white strings with Nothing.
chompInput :: String -> Either ParseError (Maybe String)
chompInput ss
 | ssTrash <- take 5 $ filter (not . Char.isPrint) ss
 , not $ null ssTrash
 = Left $ ParseError
        $ "string contains non-printable characters " ++ (show ssTrash)

 | otherwise
 = let ss1 = dropWhile Char.isSpace ss
       ss2 = reverse $ dropWhile Char.isSpace $ reverse ss1
       n   = length ss2
   in if | n == 0    -> Right Nothing
         | n >  4096 -> Left $ ParseError "string exceeds hard size limit"
         | otherwise -> Right (Just ss2)


-- | Squash empty fields to null
loadInput :: forall a. Parse a => String -> Either ParseError (Maybe a)
loadInput ss
 = do   mss <- chompInput ss
        case mss of
         Nothing  -> Right Nothing
         Just ss' -> fmap Just (parse @a ss')


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

