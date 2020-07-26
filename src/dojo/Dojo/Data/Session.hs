
module Dojo.Data.Session
        ( module Dojo.Data.Session.Base
        , module Dojo.Data.Session.Database
        , sessionOwnsEvent)
where
import Dojo.Data.Session.Base
import Dojo.Data.Session.Database
import Dojo.Data.Event



sessionOwnsEvent :: Session -> Event -> Bool
sessionOwnsEvent ss event
 =  sessionIsAdmin ss
 || (case eventCreatedBy event of
        Nothing  -> False
        Just uid -> uid == sessionUserId ss)

