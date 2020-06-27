
module Dojo.Node.PersonEdit.Arg
        ( Arg (..)
        , takeFeedForm
        , argOfKeyVal
        , keyValOfArg)
where
import Dojo.Framework
import Dojo.Base


-- | A CGI argument passed to the EventEdit node.
data Arg
        -- | Empty form field.
        = ArgEmpty

        -- | Feedback details field was just updated
        | ArgDetailsUpdated
                { argDetailsField       :: String}

        -- | Details field is invalid
        | ArgDetailsInvalid
                { argDetailsField       :: String
                , argDetailsContents    :: String
                , argDetailsError       :: String }


-- | Take feedback details from an argument.
takeFeedForm :: Arg -> Maybe FeedForm
takeFeedForm arg
 = case arg of
        ArgDetailsUpdated f
         -> Just $ FeedFormUpdated f

        ArgDetailsInvalid f c e
         -> Just $ FeedFormInvalid f c e

        _ -> Nothing


-- | Convert a CGI key value pair to an argument.
argOfKeyVal :: (String, String) -> Maybe Arg
argOfKeyVal (key, val)
        -- Details field just updated
        | "p" <- key
        = Just $ ArgDetailsUpdated val

        -- Details field invalid
        --  encoded as iFIELD=CONTENTS|ERROR
        | Just fieldName <- stripPrefix "i" key
        , (sContents, '|' : sError) <- break (== '|') val
        = Just $ ArgDetailsInvalid fieldName sContents sError

        | otherwise
        = Just ArgEmpty                                                 -- TODO: better parsing


-- | Convert an argument to a CGI key value pair.
keyValOfArg :: Arg -> (String, String)
keyValOfArg arg
 = case arg of
        ArgDetailsUpdated field
         -> ("p",   field)

        ArgDetailsInvalid field str pe
         -> ("i" ++ field, str ++ "|" ++ pe)

        _ -> error "EventEdit.keyValOfArg: nope"
