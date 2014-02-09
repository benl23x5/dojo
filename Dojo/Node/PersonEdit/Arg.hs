
module Dojo.Node.PersonEdit.Arg
        ( Arg (..)
        , takeDetailsUpdated
        , takeDetailsInvalid
        , argOfKeyVal
        , keyValOfArg)
where
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
                , argDetailsString      :: String }


-- | Take the field name from an ArgDetailsUpdated.
takeDetailsUpdated :: Arg -> Maybe String
takeDetailsUpdated arg
 = case arg of
        ArgDetailsUpdated field         -> Just field
        _                               -> Nothing


-- | Take the field and value from an ArgDetailsInvalid
takeDetailsInvalid :: Arg -> Maybe (String, String)
takeDetailsInvalid arg
 = case arg of
        ArgDetailsInvalid field val     -> Just (field, val)
        _                               -> Nothing


-- | Convert a CGI key value pair to an argument.
argOfKeyVal :: (String, String) -> Maybe Arg
argOfKeyVal (key, val)
        -- Details field just updated
        | "p" <- key
        = Just $ ArgDetailsUpdated val

        -- Details field invalid
        | Just fieldName <- stripPrefix "i" key
        = Just $ ArgDetailsInvalid fieldName val

        | otherwise            
        = Just ArgEmpty                                                 -- TODO: better parsing


-- | Convert an argument to a CGI key value pair.
keyValOfArg :: Arg -> (String, String)
keyValOfArg arg
 = case arg of
        ArgDetailsUpdated field
         -> ("p",   field)

        ArgDetailsInvalid field str
         -> ("i" ++ field, str)

        _ -> error "EventEdit.keyValOfArg: nope"
