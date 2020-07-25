
module Dojo.Fail
        ( throw
        , Fail(..))
where
import Dojo.Framework.Parse
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

        -- | Parse of a field failed.
        | FailParse
        { failParseType         :: String
        , failParseText         :: String
        , failParseError        :: ParseError }
        deriving Show

instance Exception Fail

