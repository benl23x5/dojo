
module Dojo.Node.EventView (cgiEventView) where
import Dojo.Data.Session
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Data.User
import Dojo.Paths
import Dojo.Fail
import Dojo.Chrome
import Dojo.Framework
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | Show details for a single event.
--      &eid=NAT  View the event with this id.
cgiEventView :: Session -> [(String, String)] -> CGI CGIResult
cgiEventView ss inputs
 | Just strEventId  <- lookup "eid" inputs
 , Right eid       <- parse strEventId
 = do
        -- TODO: handle concurrent event deletion.
        conn       <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        Just event <- liftIO $ getEvent conn eid

        (userCreatedBy, personCreatedBy)
         <- liftIO $ getEventCreatedBy conn event

        -- Get list of people that attended the event.
        psAttend   <- liftIO $ getAttendance conn eid
        liftIO $ disconnect conn

        cgiPageNavi (eventDisplayName event) (pathsJump ss)
         $ H.div ! A.id "event-view"
         $ do   divEventDetails ss event
                        userCreatedBy personCreatedBy
                        psAttend

                divAttendeesList ss event psAttend

cgiEventView _ inputs
 = throw $ FailNodeArgs "event view" inputs


-------------------------------------------------------------------------------
-- | Event details.
divEventDetails :: Session -> Event -> User -> Person -> [Person] -> Html
divEventDetails ss event userCreatedBy personCreatedBy attendance
 = H.div ! A.class_ "details" ! A.id "event-details-view"
 $ H.table
 $ do   tr $ td $ H.string
           $ maybe "[sometype]" (\v -> pretty v ++ " class") (eventType event)
           ++ " by "
           ++ maybe "" pretty (personDisplayName personCreatedBy)
           ++ " (" ++ pretty (userName userCreatedBy) ++ ")."

        tr $ td $ H.string
           $  maybe "[somewhere]" pretty  (eventLocation event)
           ++ maybe "[someday]"  (\v -> " on " ++ pretty v) (eventDate event)
           ++ maybe "[sometime]" (\v -> " at " ++ pretty v) (eventTime event)
           ++ "."

        -- Event can be edited by admin or the user that created it.
        when (sessionOwnsEvent ss event)
         $ tr $ td $ do
                pathLink (pathEventEditDetails ss $ eventId event)
                preEscapedToMarkup ("&nbsp;&nbsp;" :: String)

                pathLink (pathEventEditAttend ss $ eventId event)
                preEscapedToMarkup ("&nbsp;&nbsp;" :: String)

                (case eventId event of
                  Just eid | null attendance
                    -> pathLink $ pathEventDel ss eid
                  _ -> return ())

 where
        pathLink path
         = H.a  ! A.href (H.toValue path)
                $ H.toMarkup $ pathName path


-------------------------------------------------------------------------------
-- | People that attended an event.
divAttendeesList :: Session -> Event -> [Person] -> Html
divAttendeesList ss event people
 = H.div ! A.class_ "list" ! A.id "event-attendance-cur"
 $ H.table
 $ do   col' "index"; col' "Name"; col' "Fees"
        tr $ do th "#"; th "attendees"; th "fees"

        forM_ (zip [(1 :: Int)..] people) $ \(ix, person) -> tr $ do

         td $ H.toMarkup $ show ix

         td' person
          $ maybe "(person)" pretty $ personDisplayName person

         td' person
          $ case eventDate event of
                Nothing   -> "unknown"
                Just date -> pretty $ personFeeStatus date person

 where  col' c  = col ! A.class_ c

        td' person val
         | Just pid <- personId person
         = td $ (H.a ! A.href (H.toValue $ pathPersonView ss pid))
                (H.toMarkup val)

         | otherwise
         = td   (H.toMarkup val)

