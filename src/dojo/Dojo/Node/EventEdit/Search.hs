
module Dojo.Node.EventEdit.Search where
import Dojo.Node.EventEdit.Feed
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Framework

-- | Add a new person by their known person id.
addPersonByPersonId
        :: IConnection conn
        => conn
        -> Event    -- ^ Event that we're currently editing.
        -> [Person] -- ^ People we know are already attending this event.
        -> PersonId -- ^ Person id of new person to add.
        -> IO [FeedEvent]

addPersonByPersonId conn event psAttend pid
 = do
        -- Find more people based on the search string,
        -- skipping over people that are already in the list.
        let pidsSkip = mapMaybe personId psAttend
        person  <- getPerson conn pid

        if
         -- person is already in the attendees list.
         | elem pid pidsSkip
         -> do  return []

         -- person is not already an attendee, so add them.
         | Just eid <- eventId event
         -> do  insertAttendance conn eid person
                return [ FeedEventPersonAdded pid ]

         -- no event id yet.
         | otherwise
         -> do  return []



-- | Find and add attendees based on the new names.
--   The returned arguments contain feedback as to whether we found a
--   unique person based on these search terms.
addPersonByName
        :: IConnection conn
        => conn
        -> Event    -- ^ Event that we're currently editing.
        -> [Person] -- ^ People we know are already attending this event.
        -> Integer  -- ^ Index of the field containing the search string.
        -> String   -- ^ Search string.
        -> IO [FeedEvent]

addPersonByName conn event psAttend ix sQuery
 = do
        -- Find more people based on the search string,
        -- skipping over people that are already in the list.
        let pidsSkip = mapMaybe personId psAttend
        found <- findPerson conn sQuery pidsSkip

        case found of
         -- Found a unique person based on these terms.
         FoundOk person
          -- .. but the person is already an attendee, so skip.
          | elem (personId person) $ map personId psAttend
          -> return []

          -- .. and the person is not already an attendee, so add them.
          | Just eid <- eventId event
          -> do  insertAttendance conn eid person
                 return [ FeedEventPersonAdded pid
                        | Just pid <- [personId person]]

          -- .. no event id yet, skip.
          | otherwise
          -> return []

         -- Didn't find any people for these terms.
         FoundNone
          -> return [FeedEventSearchFoundNone ix sQuery]

         -- Found multiple people for these terms.
         FoundMany people
          -> return $ FeedEventSearchFoundMultiString ix sQuery
                    : map (FeedEventSearchFoundMultiPersonId ix)
                          (take 6 $ mapMaybe personId people)

