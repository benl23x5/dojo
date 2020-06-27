
module Dojo.Node.EventView
        (cgiEventView)
where
import Dojo.Data.Session
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Paths
import Dojo.Chrome
import Dojo.Framework
import Dojo.Base
import Config
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | Show details for a single event.
cgiEventView :: Session -> [(String, String)] -> CGI CGIResult
cgiEventView ss inputs
 | Just strEventId      <- lookup "eid" inputs
 , Right eid            <- parse strEventId
 = do
        -- Connect to database.
        conn    <- liftIO $ connectSqlite3 databasePath

        -- Load the event description.
        event   <- liftIO $ getEvent conn eid

        -- Load the current attendance.
        attendance <- liftIO $ getAttendance conn eid

        cgiEventView_page ss event attendance

 | otherwise
 = error $ "cgiEventView: bad inputs " ++ show inputs


cgiEventView_page
        :: Session
        -> Event -> [Person]
        -> CGI CGIResult

cgiEventView_page ss event attendance
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader $ pretty $ eventDisplayName event
        pageBody
         $ do   tablePaths $ pathsJump ss
                tablePaths $ [pathEventEdit ss $ Just $ eventId event]
                divEventView ss event attendance


-- Event ----------------------------------------------------------------------
divEventView :: Session -> Event -> [Person] -> Html
divEventView ss event attendance
 = H.div ! A.id "event-view"
 $ do
        divEventDetails event
        divPersonList ss attendance


-- | Event details.
divEventDetails :: Event -> Html
divEventDetails event
 = H.div ! A.id "event-details-view" ! A.class_ "details"
 $ do
        H.table
         $ do   col' "EventId"; col' "Date"; col' "Time"
                tr $ do th' "id"
                        th' "date (dd-mm-yyyy)"
                        th' "time (hh:mm 24hr)"

                tr $ do td' (eventId       event)
                        td' (eventDate     event)
                        td' (eventTime     event)

        H.table
         $ do   col' "Location"; col' "Type"
                tr $ do th' "location"
                        th' "type (dojo, ttc)"

                tr $ do td' (eventLocation event)
                        td' (eventType     event)

 where  col' c  = col ! A.class_ c
        th' val = th val
        td' val = td $ H.toMarkup val


-- | People that attended an event.
divPersonList :: Session -> [Person] -> Html
divPersonList ss people
 = H.div ! A.id     "event-attendance-cur"
         ! A.class_ "list"
 $ table
 $ do   col' "index"
        col' "Name"
        col' "actions"

        tr $ do th "#"
                th "attendees"

        zipWithM_ (trPerson ss) [1..] people

 where  col' c = col ! A.class_ c


-- | One person that attend an event.
trPerson :: Session -> Int -> Person -> Html
trPerson ss ix person
 = tr
 $ do   let path = pathPersonView ss (personId person)

        -- Person index in this event
        td $ H.toMarkup (show ix)

        -- Person name
        td $ H.a
           ! A.href (H.toValue path)
           $ H.toMarkup $ personDisplayName person


