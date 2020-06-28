
module Dojo.Node.EventEdit.Arg
        ( Arg (..)
        , takeFeedForm
        , argOfKeyVal
        , keyValOfArg
        , renumberSearchFeedback)
where
import Dojo.Data.Person
import Dojo.Framework
import Dojo.Base


-- | A CGI argument passed to the EventEdit node.
data Arg
        -- | Empty form field.
        = ArgEmpty

        -- | Feedback that a field has been updated.
        | ArgFeedFormUpdated
        { argDetailsFeed        :: String}

        -- | Add a person to the attendance record.
        | ArgAddPerson          String

        -- | Delete a person from the attendance record.
        | ArgDelPerson          PersonId

        -- | Person search returned no matches.
        | ArgSearchFoundNone
        { -- | Index of this search.
          argSearchIx           :: Integer

          -- | The search string.
        , argSearchString       :: String }

        -- | Person search returned multiple matches,
        --   and here is the search string.
        | ArgSearchFoundMultiString
        { -- | Index of this search.
          argSearchIx           :: Integer

          -- | The search string.
        , argSearchString       :: String }

        -- | Person search returned multiple matches,
        --   and here is one of the indices that matched.
        | ArgSearchFoundMultiPersonId
        { -- | Index of this search
          argSearchIx           :: Integer

          -- | PersonId that matched the string.
        , argSearchPersonId     :: PersonId }

        -- | Feedback a new person was just added.
        | ArgPersonAdded
        { argPersonId           :: PersonId }


-- | Take feedback details from an argument.
takeFeedForm :: Arg -> Maybe FeedForm
takeFeedForm arg
 = case arg of
        ArgFeedFormUpdated f
         -> Just $ FeedFormUpdated f

        _ -> Nothing


-- | Convert a CGI key value pair to an argument.
argOfKeyVal :: (String, String) -> Maybe Arg
argOfKeyVal (key, val)
        -- Details field just updated
        | "u" <- key
        = Just $ ArgFeedFormUpdated val

        -- Add Person
        | "addPerson"   <- key
        = if null val
                then Just $ ArgEmpty
                else Just $ ArgAddPerson val

        -- Del person
        | "delPerson"    <- key
        , Right pid      <- parse val
        = Just $ ArgDelPerson pid

        -- A person with this pid was just added
        | "a"           <- key
        , Right pid     <- parse val
        = Just $ ArgPersonAdded pid

        -- Search Found None
        | Just ns       <- stripPrefix  "sn" key
        = Just $ ArgSearchFoundNone (read ns) val

        -- Search Found Many
        | Just ns       <- stripPrefix  "sm" key
        = Just $ ArgSearchFoundMultiString (read ns) val

        | Just ns       <- stripPrefix  "sp" key
        , Right pid     <- parse val
        = Just $ ArgSearchFoundMultiPersonId (read ns) pid

        | otherwise
        = Just ArgEmpty                                                 -- TODO: better parsing


-- | Convert an argument to a CGI key value pair.
keyValOfArg :: Arg -> (String, String)
keyValOfArg arg
 = case arg of
        ArgFeedFormUpdated field
         -> ("u",   field)

        ArgPersonAdded pid
         -> ("a",   pretty pid)

        ArgSearchFoundNone ix str
         -> ("sn" ++ show ix, str)

        ArgSearchFoundMultiString ix str
         -> ("sm" ++ show ix, str)

        ArgSearchFoundMultiPersonId ix pid
         -> ("sp" ++ show ix, pretty pid)

        _ -> error "EventEdit.keyValOfArg: no match"


-- | Renumber args that provide search feedback so all the attendance
--   entries that did not match are packed to the front of the form.
renumberSearchFeedback :: [Arg] -> [Arg]
renumberSearchFeedback args
 = go 0 [] args
 where  go _ _ [] = []

        go ix alpha (ArgSearchFoundNone _ str : rest)
         = ArgSearchFoundNone ix str
         : go (ix + 1) alpha rest

        go ix alpha (ArgSearchFoundMultiString ixOld str : rest)
         = ArgSearchFoundMultiString ix str
         : go (ix + 1) ((ixOld, ix + 1) : alpha) rest

        go ix alpha (ArgSearchFoundMultiPersonId ixOld str : rest)
         = case lookup ixOld alpha of
            Nothing     -> error "renumberSearchFeedback: orphan multi pid"
            Just ixNew  -> ArgSearchFoundMultiPersonId ixNew str
                        :  go ix alpha rest

        go ix alpha (arg : rest)
         = arg : go ix alpha rest

