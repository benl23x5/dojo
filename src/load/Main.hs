
import qualified System.Environment     as S
import qualified Text.CSV               as CSV
import qualified Data.Time              as Time
import qualified Data.Time.Format       as TimeFormat
import Data.Char
import Data.List


main :: IO ()
main
 = do   [fileName] <- S.getArgs
        Right csv <- CSV.parseCSVFromFile fileName

        putStrLn
         $ "INSERT INTO v1_Person "
         ++ (parens $ intercalate ","
                [ "PersonId", "MemberId"
                , "PreferredName", "FirstName", "FamilyName"
                , "DateOfBirth"
                , "PhoneFixed", "PhoneMobile"
                , "Email"
                , "DojoHome"
                , "MembershipLevel"
                , "MembershipRenewal"])
         ++ " VALUES"

        putStr
         $ intercalate (",\n")
         $ zipWith convert [1000 .. ] $ drop 1 csv

        putStrLn ";"


-- | Convert a single row.
convert :: Integer -> [String] -> String
convert iPid
        [ sFirst, sPref, sFamily
        , sEmail
        , sPhone, sMobile
        , sBirthdate
        , sDojoHome
        , sMemberType
        , _sMemberStatus
        , sMemberRenewal]
 = (parens $ intercalate ","
        [ show iPid
        , "null"
        , cleanName             sPref
        , cleanName             sFirst
        , cleanName             sFamily
        , cleanDate             sBirthdate
        , cleanPhone            sPhone
        , cleanPhone            sMobile
        , cleanEmail            sEmail
        , cleanDojo             sDojoHome
        , cleanMemberType       sMemberType
        , cleanDate             sMemberRenewal])

convert _ _ = error "convert: failed"


-------------------------------------------------------------------------------
cleanName :: String -> String
cleanName ss
 | all isWhite ss       = "null"
 | otherwise            = quote ss


-------------------------------------------------------------------------------
cleanEmail :: String -> String
cleanEmail ss
 | all isWhite ss       = "null"
 | otherwise            = quote ss


-------------------------------------------------------------------------------
cleanDojo :: String -> String
cleanDojo ss
 | all isWhite ss       = "null"
 | otherwise            = quote ss


-------------------------------------------------------------------------------
cleanMemberType :: String -> String
cleanMemberType ss
 | all isWhite ss       = "null"
 | otherwise            = quote ss


-------------------------------------------------------------------------------
cleanPhone :: String -> String
cleanPhone ss
 | all isWhite ss
 = "null"

 | length ss == 9
 , isPrefixOf "4" ss
 = quote $ "+61" ++ ss

 | length ss == 9
 , isPrefixOf "2" ss
 = quote $ "+61" ++ ss

 | length ss == 10
 , isPrefixOf "04" ss
 = quote $ "+61" ++ drop 1 ss

 | length ss == 10
 , isPrefixOf "02" ss
 = quote $ "+61" ++ drop 1 ss

 | otherwise
 = quote $ ss


-------------------------------------------------------------------------------
cleanDate :: String -> String
cleanDate str
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

  | length str == 0
  = "null"

  | otherwise
  = error "cannot parse date: " ++ show str

  where check nDay nMonth nYear
         | not $ nYear >= 1900
         = error "date must be after 1900"

         | not $ nYear <  2100
         = error "date must be before 2100"

         | otherwise
         = case Time.fromGregorianValid nYear nMonth nDay of
            Just date
             -> quote $ TimeFormat.formatTime
                        TimeFormat.defaultTimeLocale
                        "%F" date
            Nothing
             -> error "date must be valid in Gregorian calendar"


-------------------------------------------------------------------------------
parens ss = "(" ++ ss ++ ")"
quote  ss = "'" ++ ss ++ "'"
isWhite c = elem c [' ', '\n', '\t']