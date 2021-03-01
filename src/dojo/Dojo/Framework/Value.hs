
module Dojo.Framework.Value where
import qualified Data.Time as Time


data Value
        = B Bool
        | I Integer
        | D Double
        | S String
        | O [(String, Value)]
        | A [Value]
        | TD Time.Day
        | TT Time.TimeOfDay
        | TL Time.LocalTime

        deriving Show

string :: String -> Value
string s = S s

class ToValue a where
 toValue :: a -> Value

instance ToValue Bool where
 toValue b = B b



