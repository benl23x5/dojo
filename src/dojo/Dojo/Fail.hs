
module Dojo.Fail
        ( throw
        , Fail(..))
where
import Control.Exception

data Fail
        -- | Tried to access a node with invalid arguments.
        = FailNodeArgs
        { failNodeName          :: String
        , failNodeArgs          :: [(String, String)] }

        -- | Lookup of entity failed.
        | FailUnknownEntity
        { failEntityType        :: String
        , failEntityId          :: String }
        deriving Show

instance Exception Fail

