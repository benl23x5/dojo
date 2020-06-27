
module Dojo.Node.PersonEdit.Arg
        ( Arg
        , argOfKeyVal
        , keyValOfArg)
where


-- | A CGI argument passed to the EventEdit node.
data Arg


-- | Convert a CGI key value pair to an argument.
argOfKeyVal :: (String, String) -> Maybe Arg
argOfKeyVal (_key, _val)
        = Nothing


-- | Convert an argument to a CGI key value pair.
keyValOfArg :: Arg -> (String, String)
keyValOfArg _arg
 = error "PersonEdit.keyValOfArg: nope"