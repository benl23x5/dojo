
module Dojo.Node.EventEdit.Feed
        ( FeedEvent(..)
        , renumberSearchFeedback
        , expandMultiPersonId)
where
import Dojo.Data.Person
import Dojo.Base

-- | Feedback for event edit form.
--
--   Actions on the event edit form can create user feedbacm that
--   should be displayed inline in the form. This type describes
--   the possible feedback.
--
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

        -- | Person search returned multiple matches,
        --   and here is full details of a record that matched.
        --   These are an expanded form of "MultiPersonId".
        | FeedEventSearchFoundMultiPerson
        { -- | Index of this search
          feedSearchIx          :: Integer

          -- | Full person record that matched.
        , feedSearchPerson      :: Person }
        deriving Show


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
         : go (ix + 1) ((ixOld, ix) : alpha) rest

        go ix alpha (FeedEventSearchFoundMultiPersonId ixOld str : rest)
         = case lookup ixOld alpha of
            Nothing     -> error "renumberSearchFeedback: orphan multi pid"
            Just ixNew  -> FeedEventSearchFoundMultiPersonId ixNew str
                        :  go ix alpha rest

        go ix alpha (FeedEventSearchFoundMultiPerson ixOld person : rest)
         = case lookup ixOld alpha of
            Nothing     -> error "renumberSearchFeedback: orphan multi pid"
            Just ixNew  -> FeedEventSearchFoundMultiPerson ixNew person
                        :  go ix alpha rest

        go ix alpha (arg : rest)
         = arg : go ix alpha rest


-- | Look up details of FoundMultiPersonId and add matching
--   FoundMultiPerson records.
expandMultiPersonId
        :: IConnection conn
        => conn
        -> FeedEvent
        -> IO [FeedEvent]

expandMultiPersonId conn fe
 = case fe of
        FeedEventSearchFoundMultiPersonId ix pid
         -> do  Just person <- getPerson conn pid
                return [fe, FeedEventSearchFoundMultiPerson ix person]

        _ -> return [fe]
