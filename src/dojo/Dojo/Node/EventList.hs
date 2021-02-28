
module Dojo.Node.EventList where
import Dojo.Data.Session
import Dojo.Data.Event
import Dojo.Paths
import Dojo.Chrome
import Dojo.Framework
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | A table of people with links to the per-person pages.
cgiEventList :: Session -> [(String, String)] -> CGI CGIResult
cgiEventList ss _inputs
 = do   conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        events  <- liftIO $ getEventList conn
        liftIO $ disconnect conn

        cgiPageNavi ss "Events" "Events" (pathsJump ss)
         $ do   when (sessionIsAdmin ss)
                 $ tableActions [pathEventAdd ss]

                divEventList ss events


-------------------------------------------------------------------------------
-- | Build the events table.
divEventList :: Session -> [(Event, Int)] -> Html
divEventList ss eventList
 = H.div ! A.class_ "list event-list"
 $ H.table
 $ do   col' "Date"; col' "Time"; col' "Location"; col' "Pax"

        H.tr $ do H.th "date"
                  H.th "time"
                  H.th "location"
                  (H.th ! A.style "text-align: center") "people"

        forM_ eventList $ \(event, pax) -> H.tr $ do
         td' event (eventDate event)
         td' event (eventTime event)
         td' event (eventLocation event)

         (H.td ! A.style "text-align: center")
          $ linkView event (H.string $ show pax)


 where  col' c  = H.col ! A.class_ c

        td' event val
         = H.td $ linkView event (H.toMarkup $ maybe "" pretty val)

        linkView event hh
         | Just eid <- eventId event
         = (H.a ! A.href (H.toValue $ pathEventView ss eid)) hh
         | otherwise = hh

