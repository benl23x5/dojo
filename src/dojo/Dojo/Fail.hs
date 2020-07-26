
module Dojo.Fail
        ( throw
        , Fail(..))
where
import Dojo.Framework.Parse
import Control.Exception

data Fail
        -- | Tried to access an unknown node
        = FailNodeUnknown
        { failNodeName          :: String }

        -- | Tried to access a node with invalid arguments.
        | FailNodeArgs
        { failNodeName          :: String
        , failNodeArgs          :: [(String, String)] }

        -- | Need session id to access this node.
        | FailSessionRequired
        { failNodeName          :: String }

        -- | Provided session hash is invalid or already logged out.
        | FailSessionInvalid
        { failSessionHash       :: String }

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

