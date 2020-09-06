
module Dojo.Node.EventEdit.Arg
        ( Arg(..)
        , FeedEvent(..)
        , argOfKeyVal
        , takeKeyValOfFeedEvent
        , takeFeedEventOfKeyVal
        , takeFeedFormOfArg)
where
import Dojo.Node.EventEdit.Feed
import Dojo.Data.Person
import Dojo.Data.Event


-- | A CGI argument passed to the EventEdit node.
data Arg
        -- | Update a field
        = ArgUpdate     String String

        -- | Add a person to the addendance list,
        --   base on the provided person id.
        | ArgAddPerson  PersonId

        -- | Delete a person from the attendance list,
        --   based on the provided person id.
        | ArgDelPerson  PersonId

        -- | Add a named person to the attendance record.
        | ArgAddName    String

        -- | Event edit feedback
        | ArgFeedEvent  FeedEvent

        -- | Empty update field, safe to ignore.
        | ArgEmpty
        deriving Show


-- | Parse a CGI key value pair to an argument.
argOfKeyVal :: (String, String) -> Maybe Arg
argOfKeyVal kv@(key, val)
        -- Add a person based on their pid.
        | "addPerson"   <- key
        , Right pid     <- parse val
        = Just $ ArgAddPerson pid

        -- Delete a person based on their pid.
        | "delPerson"   <- key
        , Right pid     <- parse val
        = Just $ ArgDelPerson pid

        -- Add a person based on their name.
        | "addName"     <- key
        = if null val
                then Just $ ArgEmpty
                else Just $ ArgAddName val

        | Just fe <- takeFeedEventOfKeyVal kv
        = Just $ ArgFeedEvent fe

        | elem key $ tableFieldNamesOfEntity eventEntity
        = Just $ ArgUpdate key val

        | otherwise
        = Nothing


-- | Convert an argument to a CGI key value pair.
takeKeyValOfFeedEvent :: FeedEvent -> Maybe (String, String)
takeKeyValOfFeedEvent fe
 = case fe of
        FeedEventFieldUpdated field
         -> Just ("fu",   field)

        FeedEventPersonAdded pid
         -> Just ("fa",   pretty pid)

        FeedEventSearchFoundNone ix str
         -> Just ("sn" ++ show ix, str)

        FeedEventSearchFoundMultiString ix str
         -> Just ("sm" ++ show ix, str)

        FeedEventSearchFoundMultiPersonId ix pid
         -> Just ("sp" ++ show ix, pretty pid)

        -- FoundMultiPerson is generated from FoundMultiPersonId
        -- interally and does not need to be passed back as an argument.
        FeedEventSearchFoundMultiPerson{}
         -> Nothing


-- | Parse a CGI key/value pair as a `FeedEvent`
--   TODO: error on unrecognized
takeFeedEventOfKeyVal :: (String, String) -> Maybe FeedEvent
takeFeedEventOfKeyVal (key, val)
        -- Details field of this name was updated.
        | "fu"          <- key
        = Just $ FeedEventFieldUpdated val

        -- A person with this pid was added.
        | "fa"          <- key
        , Right pid     <- parse val
        = Just $ FeedEventPersonAdded pid

        -- Search found no matches.
        | Just ns       <- stripPrefix "sn" key
        = Just $ FeedEventSearchFoundNone (read ns) val

        -- Search found multi matches, and here is the search string.
        | Just ns       <- stripPrefix "sm" key
        = Just $ FeedEventSearchFoundMultiString (read ns) val

        -- Search found multi matches, and here is a matching person id.
        | Just ns       <- stripPrefix "sp" key
        , Right pid     <- parse val
        = Just $ FeedEventSearchFoundMultiPersonId (read ns) pid

        | otherwise
        = Nothing


-- | Take feedback details from an argument.
takeFeedFormOfArg :: Arg -> Maybe FeedForm
takeFeedFormOfArg arg
 = case arg of
        ArgFeedEvent (FeedEventFieldUpdated f)
         -> Just $ FeedFormUpdated f

        _ -> Nothing

