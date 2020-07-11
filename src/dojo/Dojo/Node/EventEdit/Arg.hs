
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
import Dojo.Framework


-- | A CGI argument passed to the EventEdit node.
data Arg
        -- | Empty form field.
        = ArgEmpty

        -- | Add a person to the attendance record.
        | ArgAddPerson          String

        -- | Delete a person from the attendance record.
        | ArgDelPerson          PersonId

        -- | Event edit feedback
        | ArgFeedEvent          FeedEvent


-- | Parse a CGI key value pair to an argument.
argOfKeyVal :: (String, String) -> Maybe Arg
argOfKeyVal kv@(key, val)
        -- Add Person
        | "addPerson"   <- key
        = if null val
                then Just $ ArgEmpty
                else Just $ ArgAddPerson val

        -- Del person
        | "delPerson"   <- key
        , Right pid     <- parse val
        = Just $ ArgDelPerson pid

        | Just fe       <- takeFeedEventOfKeyVal kv
        = Just $ ArgFeedEvent fe

        -- TODO: better parsing
        | otherwise
        = Just ArgEmpty


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
--   TODO: safe read.
--   TODO: error on unrecognized
takeFeedEventOfKeyVal :: (String, String) -> Maybe FeedEvent
takeFeedEventOfKeyVal (key, val)
        -- Details field of this name was updated.
        | "fu" <- key
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



