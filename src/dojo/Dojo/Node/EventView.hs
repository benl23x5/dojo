
module Dojo.Node.EventView (cgiEventView) where
import Dojo.Data.Session
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Paths
import Dojo.Fail
import Dojo.Chrome
import Config
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | Show details for a single event.
--      &eid=NAT  View the event with this id.
cgiEventView
        :: Session -> [(String, String)]
        -> CGI CGIResult

cgiEventView ss inputs
 | Just strEventId  <- lookup "eid" inputs
 , Right eid        <- parse strEventId
 = do   conn    <- liftIO $ connectSqlite3 databasePath
        event   <- liftIO $ getEvent conn eid
        attend  <- liftIO $ getAttendance conn eid
        liftIO $ disconnect conn

        cgiEventView_page ss event attend

 | otherwise
 = throw $ FailNodeArgs "event view" inputs


cgiEventView_page ss event attendance
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader $ pretty $ eventDisplayName event
        pageBody
         $ do   tablePaths $ pathsJump ss
                tablePaths $ [pathEventEdit ss $ eventId event]
                H.div ! A.id "event-view"
                 $ do   divEventDetails event
                        divPersonList ss attendance


-------------------------------------------------------------------------------
-- | Event details.
divEventDetails :: Event -> Html
divEventDetails event
 = H.div ! A.class_ "details" ! A.id "event-details-view"
 $ do
        H.table
         $ do   col' "EventId"; col' "Date"; col' "Time"
                tr $ do th "id"; th "date"; th "time"

                tr $ do td' (eventId       event)
                        td' (eventDate     event)
                        td' (eventTime     event)

        H.table
         $ do   col' "Location"; col' "Type"
                tr $ do th "location"; th "type"

                tr $ do td' (eventLocation event)
                        td' (eventType     event)

 where  col' c  = col ! A.class_ c
        td' val = td $ H.toMarkup $ maybe "" pretty $ val


-------------------------------------------------------------------------------
-- | People that attended an event.
divPersonList :: Session -> [Person] -> Html
divPersonList ss people
 = H.div ! A.class_ "list" ! A.id "event-attendance-cur"
 $ H.table
 $ do   col' "index"; col' "Name"
        tr $ do th "#"; th "attendees"

        forM_ (zip [(1 :: Int)..] people) $ \(ix, person) -> tr $ do
         td $ H.toMarkup $ show ix
         td' person $ pretty $ personDisplayName person

 where  col' c  = col ! A.class_ c

        td' person val
         | Just pid <- personId person
         = td $ (H.a ! A.href (H.toValue $ pathPersonView ss pid))
                (H.toMarkup val)

         | otherwise
         = td   (H.toMarkup val)

