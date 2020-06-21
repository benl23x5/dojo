
import qualified System.Environment     as S
import qualified Text.CSV               as CSV
import Data.List

main :: IO ()
main = do
        [fileName] <- S.getArgs
        Right csv <- CSV.parseCSVFromFile fileName
        mapM_ putStrLn
         $ zipWith convert [1000 .. ] $ drop 1 csv


-- TODO: Prefered => Preferred
convert :: Integer -> [String] -> String
convert iPid
        [ sFirst, sPref, sFamily, sEmail
        , sPhone, sMobile
        , sBirthdate
        , sDojo
        , sMemberType, sMemberStatus, sMemberExpiry]
 =  "insert into Person"
 ++ (parens $ intercalate ","
        [ "PersonId", "MemberId"
        , "PreferedName", "FirstName", "MiddleName", "FamilyName"
        , "DateOfBirth"
        , "Mobile"
        , "Email"])
 ++ "values"
 ++ (parens $ intercalate ","
        [ show iPid, quote "0"
        , quote sPref, quote sFirst, quote "", quote sFamily
        , "null"
        , quote sMobile'
        , quote sEmail])
 ++ ";"
 where
        sMobile'
         = cleanPhone
         $ if not (sMobile == "")
                then sMobile
                else sPhone

cleanPhone :: String -> String
cleanPhone ss
 | length ss == 9
 , isPrefixOf "4" ss
 = "+61" ++ ss

 | length ss == 9
 , isPrefixOf "2" ss
 = "+61" ++ ss

 | length ss == 10
 , isPrefixOf "04" ss
 = "+61" ++ drop 1 ss

 | length ss == 10
 , isPrefixOf "02" ss
 = "+61" ++ drop 1 ss

 | otherwise
 = ss

parens ss = "(" ++ ss ++ ")"
quote  ss = "'" ++ ss ++ "'"