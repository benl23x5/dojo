
module Dojo.Node.EventEdit.Feed
        ( FeedEvent(..)
        , renumberSearchFeedback)
where
import Dojo.Data.Person


-- | Feedback from event entry form.
data FeedEvent
        = FeedEventFieldUpdated
        { feedField             :: String }

        | FeedEventPersonAdded
        { feedPersonId          :: PersonId }

        -- | Person search returned no matches.
        | FeedEventSearchFoundNone
        { -- | Index of this search.
          feedSearchIx          :: Integer

          -- | The search string.
        , feedSearchString      :: String }

        -- | Person search returned multiple matches,
        --   and here is the search string.
        --   There should also be some 'FoundMultiPersonId' feedback.
        | FeedEventSearchFoundMultiString
        { -- | Index of this search.
          feedSearchIx          :: Integer

          -- | The search string.
        , feedSearchString      :: String }

        -- | Person search returned multiple matches,
        --   and here is one of the indices that matched.
        --   There should also be some 'FoundMultiString' feedback.
        | FeedEventSearchFoundMultiPersonId
        { -- | Index of this search
          feedSearchIx          :: Integer

          -- | PersonId that matched the string.
        , feedSearchPersonId    :: PersonId }


-- | Renumber args that provide search feedback so all the attendance
--   entries that did not match are packed to the front of the form.
renumberSearchFeedback
        :: [FeedEvent] -> [FeedEvent]

renumberSearchFeedback args
 = go 0 [] args
 where  go _ _ [] = []

        go ix alpha (FeedEventSearchFoundNone _ str : rest)
         = FeedEventSearchFoundNone ix str
         : go (ix + 1) alpha rest

        go ix alpha (FeedEventSearchFoundMultiString ixOld str : rest)
         = FeedEventSearchFoundMultiString ix str
         : go (ix + 1) ((ixOld, ix + 1) : alpha) rest

        go ix alpha (FeedEventSearchFoundMultiPersonId ixOld str : rest)
         = case lookup ixOld alpha of
            Nothing     -> error "renumberSearchFeedback: orphan multi pid"
            Just ixNew  -> FeedEventSearchFoundMultiPersonId ixNew str
                        :  go ix alpha rest

        go ix alpha (arg : rest)
         = arg : go ix alpha rest