
module Dojo.Node.EventList where
import Dojo.Data.Session
import Dojo.Data.Event
import Dojo.Paths
import Dojo.Chrome
import Config
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | A table of people, with edit links on the right.
cgiEventList :: Session -> [(String, String)] -> CGI CGIResult
cgiEventList ss _inputs
 = do   -- Connect to the database.
        conn    <- liftIO $ connectSqlite3 databasePath

        -- Read current data for all people
        eventList  <- liftIO $ getEventList conn
        liftIO $ disconnect conn

        cgiEventList_list ss eventList


cgiEventList_list ss eventList
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Events"
        pageBody
         $ do   tablePaths $ pathsJump ss
                tablePaths [pathEventAdd ss]
                tableEventList ss eventList


-- | Build the events table.
tableEventList :: Session -> [(Event, Int)] -> Html
tableEventList ss eventList
 = H.div ! A.class_ "list event-list"
 $ H.table
 $ do   col' "Date"; col' "Time"; col' "Location"; col' "Pax"
        tr $ do th "date"; th "time"; th "location"; th "pax"

        mapM_ (trEvent ss) eventList

 where  col' c  = col ! A.class_ c


-- | Build one row of the event table.
trEvent :: Session -> (Event, Int) -> Html
trEvent ss (event, peopleCount)
 = tr
 $ do   -- TODO: supress link on no eid
        let Just eid = eventId event
        let pathView = pathEventView ss eid

        let tdField val
             = td $ (a ! A.href (H.toValue pathView))
                        (H.toMarkup val)

        -- Event data.
        tdField (fromMaybe "" $ fmap pretty $ eventDate      event)
        tdField (fromMaybe "" $ fmap pretty $ eventTime      event)
        tdField (fromMaybe "" $ fmap pretty $ eventLocation  event)
        tdField peopleCount

