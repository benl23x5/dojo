
import qualified System.Environment     as S
import qualified Text.CSV               as CSV
import qualified Data.Time              as Time
import qualified Data.Time.Format       as TimeFormat
import Data.Maybe
import Data.Char
import Data.List


-------------------------------------------------------------------------------
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
        , fromMaybe "null" $ fmap quote $ cleanName sPref
        , fromMaybe "null" $ fmap quote $ cleanName sFirst
        , fromMaybe "null" $ fmap quote $ cleanName sFamily
        , fromMaybe "null" $ fmap quote $ cleanDate sBirthdate
        , fromMaybe "null" $ fmap quote $ sFixed'
        , fromMaybe "null" $ fmap quote $ sMobile'
        , fromMaybe "null" $ fmap quote $ cleanEmail sEmail
        , fromMaybe "null" $ fmap quote $ cleanDojo  sDojoHome
        , fromMaybe "null" $ fmap quote $ cleanMemberType sMemberType
        , fromMaybe "null" $ fmap quote $ cleanDate sMemberRenewal])
 where
        (sFixed', sMobile')
         = cleanPhones sPhone sMobile

convert _ _ = error "convert: failed"


-------------------------------------------------------------------------------
cleanName :: String -> Maybe String
cleanName ss
 | all isWhite ss       = Nothing
 | otherwise            = Just ss


-------------------------------------------------------------------------------
cleanEmail :: String -> Maybe String
cleanEmail ss
 | all isWhite ss       = Nothing
 | otherwise            = Just ss


-------------------------------------------------------------------------------
cleanDojo :: String -> Maybe String
cleanDojo ss
 | all isWhite ss       = Nothing
 | ss == "Unallocated"  = Nothing
 | otherwise            = Just ss


-------------------------------------------------------------------------------
cleanMemberType :: String -> Maybe String
cleanMemberType ss
 | all isWhite ss                       = Nothing

 | isPrefixOf "1x " ss                  = cleanMemberType $ drop 3 ss
 | isPrefixOf "Test level" ss           = Nothing

 | isPrefixOf "Special" ss              = Just $ "Special"

 | isPrefixOf "Family Annual" ss        = Just $ "Family Annual"
 | isPrefixOf "Adult Annual" ss         = Just $ "Adult Annual"
 | isPrefixOf "University Annual" ss    = Just $ "University Annual"
 | isPrefixOf "Student Annual" ss       = Just $ "Student Annual"
 | isPrefixOf "Concession Annual" ss    = Just $ "Concession Annual"
 | isPrefixOf "Child Annual" ss         = Just $ "Child Annual"

 | isPrefixOf "Family Intro" ss         = Just $ "Family Intro (3 months)"
 | isPrefixOf "Adult Intro" ss          = Just $ "Adult Intro (3 months)"
 | isPrefixOf "University Intro" ss     = Just $ "University Intro (3 months)"
 | isPrefixOf "Student Intro" ss        = Just $ "Student Intro (3 months)"
 | isPrefixOf "Concession Intro" ss     = Just $ "Concession Intro (3 months)"
 | isPrefixOf "Child Intro" ss          = Just $ "Child Intro (3 months)"

 | otherwise                            = Just ss


-------------------------------------------------------------------------------
data PhoneType
        = PhoneUnknown
        | PhoneFixed
        | PhoneMobile
        deriving (Eq, Show)


-- | Classify a phone number by type.
classifyPhone :: String -> PhoneType
classifyPhone ss
 | isPrefixOf "+612" ss = PhoneFixed
 | isPrefixOf "+614" ss = PhoneMobile
 | otherwise            = PhoneUnknown


-- | Syntactic cleaning of an Australian phone number.
cleanPhone :: String -> Maybe String
cleanPhone ss_
 | all isWhite ss
 = Nothing

 -- Assume NSW number with no area code.
 | length ss == 8,  isPrefixOf "9"  ss = Just $ "+612" ++ ss
 | length ss == 8,  isPrefixOf "4"  ss = Just $ "+612" ++ ss

 -- Assume Australian area code without leading zero.
 | length ss == 9,  isPrefixOf "2"  ss = Just $ "+61" ++ ss
 | length ss == 9,  isPrefixOf "3"  ss = Just $ "+61" ++ ss
 | length ss == 9,  isPrefixOf "4"  ss = Just $ "+61" ++ ss
 | length ss == 9,  isPrefixOf "7"  ss = Just $ "+61" ++ ss
 | length ss == 9,  isPrefixOf "8"  ss = Just $ "+61" ++ ss

 -- Assume Australian area code with leading zero.
 | length ss == 10, isPrefixOf "02" ss = Just $ "+61" ++ drop 1 ss
 | length ss == 10, isPrefixOf "03" ss = Just $ "+61" ++ drop 1 ss
 | length ss == 10, isPrefixOf "04" ss = Just $ "+61" ++ drop 1 ss
 | length ss == 10, isPrefixOf "07" ss = Just $ "+61" ++ drop 1 ss
 | length ss == 10, isPrefixOf "08" ss = Just $ "+61" ++ drop 1 ss

 -- Assume Australian country code without '+' prefix.
 | length ss == 11, isPrefixOf "61" ss = Just $ "+" ++ ss

 | otherwise
 = Just $ ss
 where  ss = filter (not . isWhite) ss_


-- | We have fixed and mobile phone number fields,
--   but sometimes the numbers are swapped, or duplicated,
--   which we sort out here.
cleanPhones :: String -> String -> (Maybe String, Maybe String)
cleanPhones sPhone sMobile
 = let  mPhone'  = cleanPhone sPhone
        mMobile' = cleanPhone sMobile

        mtPhone  = fmap classifyPhone mPhone'
        mtMobile = fmap classifyPhone mMobile'

   in   if | mPhone' == mMobile'
           , mtPhone == Just PhoneFixed
           -> (mPhone', Nothing)

           | mPhone' == mMobile'
           , mtPhone == Just PhoneMobile
           -> (Nothing, mMobile')

           |  mtPhone  == Just PhoneMobile
           || mtMobile == Just PhoneFixed
           -> (mMobile', mPhone')

           | otherwise
           -> (mPhone',  mMobile')


-------------------------------------------------------------------------------
cleanDate :: String -> Maybe String
cleanDate str
  | length str == 0
  = Nothing

  | str == "1 Jan 1900"
  = Nothing

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
  , Just nMonth
    <- lookup sMonth
     $ zip [ "Jan", "Feb", "Mar", "Apr", "May", "Jun"
           , "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"] [1..]
  , length sYear > 0
  = check (read sDay) nMonth (read sYear)

  | otherwise
  = error $ "cannot parse date: " ++ show str

  where check nDay nMonth nYear
         | not $ nYear >= 1900
         = error "date must be after 1900"

         | not $ nYear <  2100
         = error "date must be before 2100"

         | otherwise
         = case Time.fromGregorianValid nYear nMonth nDay of
            Just date
             -> Just $ TimeFormat.formatTime
                        TimeFormat.defaultTimeLocale
                        "%F" date
            Nothing
             -> error "date must be valid in Gregorian calendar"


-------------------------------------------------------------------------------
parens ss = "(" ++ ss ++ ")"
quote  ss = "'" ++ ss ++ "'"
isWhite c = elem c [' ', '\n', '\t']