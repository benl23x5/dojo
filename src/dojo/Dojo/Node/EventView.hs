
module Dojo.Node.EventView (cgiEventView) where
import Dojo.Data.Session
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Node.EventEdit.Details
import Dojo.Paths
import Dojo.Fail
import Dojo.Chrome
import Dojo.Framework
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Data.List                      as List


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
        psAttend   <- fmap (List.sortOn personDisplayName)
                   $  liftIO $ getAttendance conn eid
        liftIO $ disconnect conn

        cgiPageNavi ss "Events" (eventDisplayName event) (pathsJump ss)
         $ H.div ! A.class_ "event-view"
         $ do
                -- Only bother showing site user name to admin users.
                let mUserCreatedBy
                        = if sessionIsAdmin ss
                                then Just userCreatedBy
                                else Nothing

                divEventDescription
                        event mUserCreatedBy personCreatedBy

                if (sessionOwnsEvent ss event)
                 then tableActions
                        $  [ pathEventEditDetails ss (Just eid)
                           , pathEventEditAttend  ss (Just eid) ]
                        ++ (if | null psAttend -> [pathEventDel ss eid]
                               | otherwise     -> [])
                 else H.br

                divAttendeesList ss event psAttend

cgiEventView _ inputs
 = throw $ FailNodeArgs "event view" inputs



-------------------------------------------------------------------------------
-- | People that attended an event.
divAttendeesList :: Session -> Event -> [Person] -> Html
divAttendeesList ss event people
 = H.div ! A.class_ "list" ! A.id "event-attendance-cur"
 $ H.table
 $ do   col' "index"; col' "Name"; col' "Fees"
        tr $ do th "#"
                th "attendees"
                th "fees" ! A.style "text-align: center"

        forM_ (zip [(1 :: Int)..] people) $ \(ix, person) -> tr $ do

         td $ H.toMarkup $ show ix

         td' person
          $ personDisplayName person

         let Just pid = personId person
         (H.td ! A.style "text-align: center")
          $ (H.a ! A.href (H.toValue $ pathPersonView ss pid))
          $ case eventDate event of
                Nothing   -> H.string "unknown"
                Just date -> H.string $ pretty $ personFeeStatus date person

 where  col' c  = col ! A.class_ c

        td' person val
         = td $ linkView person (H.toMarkup $ maybe "" pretty val)

        linkView person hh
         | Just pid <- personId person
         = (a ! A.href (H.toValue $ pathPersonView ss pid)) hh
         | otherwise = hh