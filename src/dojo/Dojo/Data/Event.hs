
module Dojo.Data.Event
        ( module Dojo.Data.Event.Base
        , module Dojo.Data.Event.Database
        , module Dojo.Data.Event.Presentation
        , summarizeEventTypes
        , getEventCreatedBy)
where
import Dojo.Data.Event.Base
import Dojo.Data.Event.Database
import Dojo.Data.Event.Presentation
import Dojo.Data.Person
import Dojo.Data.User
import Dojo.Framework

import qualified Data.Map.Strict        as Map
import Data.Map.Strict                  (Map)


-- | Summarize how many events are in each type.
summarizeEventTypes :: [Event] -> Map EventType Integer
summarizeEventTypes events
 = foldl' (Map.unionWith (+)) Map.empty
 $ catMaybes
 $ [ case eventType event of
        Nothing -> Nothing
        Just et -> Just $ Map.singleton et 1
   | event <- events ]


-- | Get the user and person that created a event.
getEventCreatedBy :: IConnection conn => conn -> Event -> IO (User, Person)
getEventCreatedBy conn event
 = do   let Just uidCreatedBy = eventCreatedBy event
        Just user <- liftIO $ getUserOfId conn uidCreatedBy
        person    <- liftIO $ getPerson   conn $ userPersonId user
        return (user, person)
