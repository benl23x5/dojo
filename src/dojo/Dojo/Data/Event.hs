
module Dojo.Data.Event
        ( module Dojo.Data.Event.Base
        , module Dojo.Data.Event.Database
        , module Dojo.Data.Event.Presentation
        , module Dojo.Framework
        , summarizeEventTypes)
where
import Dojo.Data.Event.Base
import Dojo.Data.Event.Database
import Dojo.Data.Event.Presentation
import Dojo.Framework
import Dojo.Trivia

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

