
module Dojo.Framework.Value where


data Value
        = B Bool
        | I Integer
        | D Double
        | S String
        | O [(String, Value)]
        | A [Value]
        deriving Show

string :: String -> Value
string s = S s

class MakeValue a where
 value :: a -> Value

instance MakeValue Bool where
 value b = B b




