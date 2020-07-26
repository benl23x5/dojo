
module Dojo.Node.EventList where
import Dojo.Data.Session
import Dojo.Data.Event
import Dojo.Paths
import Dojo.Chrome
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | A table of people with links to the per-person pages.
--      no args.
cgiEventList
        :: Session -> [(String, String)]
        -> CGI CGIResult

cgiEventList ss _inputs
 = do   conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        events  <- liftIO $ getEventList conn
        liftIO $ disconnect conn
        cgiEventList_list ss events


cgiEventList_list ss events
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Events"
        pageBody
         $ do   tablePaths $ pathsJump ss

                when (sessionIsAdmin ss)
                 $ tablePaths [pathEventAdd ss]

                divEventList ss events


-------------------------------------------------------------------------------
-- | Build the events table.
divEventList :: Session -> [(Event, Int)] -> Html
divEventList ss eventList
 = H.div ! A.class_ "list event-list"
 $ H.table
 $ do   col' "Date"; col' "Time"; col' "Location"; col' "Pax"
        tr $ do th "date"; th "time"; th "location"; th "pax"

        forM_ eventList $ \(event, pax) -> tr $ do
         td' event (eventDate event)
         td' event (eventTime event)
         td' event (eventLocation event)
         td' event (Just $ show pax)

 where  col' c  = col ! A.class_ c

        td' event val
         | Just eid <- eventId event
         = td $ (a ! A.href (H.toValue $ pathEventView ss eid))
                (H.toMarkup $ maybe "" pretty val)

         | otherwise
         = td $ (H.toMarkup $ maybe "" pretty val)

