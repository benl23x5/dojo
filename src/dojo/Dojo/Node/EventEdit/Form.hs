
module Dojo.Node.EventEdit.Form
        (formEvent)
where
import Dojo.Node.EventEdit.Arg
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Framework
import Dojo.Base
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | Form to change the details of a single event.
--    We don't allow the eventid to be edited because this is the primary
--    key for the person table.
formEvent
        :: Path -> [FeedForm] -> [FeedEvent]
        -> Event -> [Person] -> Html

formEvent path fsForm fsEvent event attendance
 = form ! A.action (H.toValue path)
 $ do
        -- Stash args from the target path as hidden fields.
        mapM_   (\(fieldName, fieldData)
                 -> input ! A.type_ "hidden"
                          ! A.name  (H.toValue fieldName)
                          ! A.value (H.toValue fieldData))
                (pathFields path)

        -- Feedback about updated and invalid fields.
        htmlFeedForm fsForm niceNameOfEventField

        -- Event details.
        divEventDetails    fsForm event
        divEventAttendance path fsForm fsEvent event attendance
        H.br

        -- Save button.
        input   ! A.type_  "submit"
                ! A.class_ "buttonFull"
                ! A.value  "Save"


-------------------------------------------------------------------------------
divEventDetails :: [FeedForm] -> Event -> Html
divEventDetails fsFeed event
 = H.div ! A.id "event-details-edit" ! A.class_ "details"
 $ do
        tableFields fsFeed
         [ ( "Date", "date (dd-mm-yyyy)"
           , pretty $ eventDate event, Just "(required)"
           , False)

         , ( "Time", "time (hh:mm 24hr)"
           , pretty $ eventTime event, Just "(required)"
           , False)
         ]

        -- When this is a new event put focus on the location input field,
        -- otherwise allow focus to be taken by the last person entry field.
        let EventId eid = eventId event

        tableFields fsFeed
         [ ( "Location", "location"
           , pretty $ eventLocation event, Just "(required)"
           , eid == 0)

         , ( "Type", "type (dojo, ttc etc)"
           , pretty $ eventType event, Just "(required)"
           , False)
         ]


-------------------------------------------------------------------------------
divEventAttendance
        :: Path -> [FeedForm] -> [FeedEvent]
        -> Event -> [Person] -> Html

divEventAttendance path fsForm fsEvent event attendance
 = do
        divAttendanceCur path fsEvent attendance

        let curStudents = fromIntegral $ length attendance
        divAttendanceNew fsForm fsEvent event curStudents


-------------------------------------------------------------------------------
-- | Table of people currently listed as attending the event.
divAttendanceCur path fsEvent attendance
 = H.div ! A.id     "event-attendance-cur"
         ! A.class_ "list"
 $ H.table
 $ do   col ! A.class_ "index"
        col ! A.class_ "name"
        col ! A.class_ "actions"

        tr $ do th "#"
                th "attendees"
                th "del"

        -- Highlight the people that were just added.
        let pidsAdded = [pid | FeedEventPersonAdded pid <- fsEvent ]

        zipWithM_ (trCurAttendance pidsAdded path) [1..] attendance


-- | Row for person that is currently listed as attending the event.
trCurAttendance :: [PersonId] -> Path -> Int -> Person -> Html
trCurAttendance pidsAdded path ix person
 = tr
 $ do   td $ H.toMarkup (show ix)

        let bJustAdded
             = case personId person of
                Nothing  -> False
                Just pid -> elem pid pidsAdded

        td !? (bJustAdded, A.class_, "updated")
           $ H.toMarkup
           $ personDisplayName person

        -- TODO: fix case of no pid
        let Just pid = personId person
        td $ H.a ! A.class_ "link"
                 ! (A.href $ H.toValue
                           $ path <&> [("delPerson", pretty pid)])
                 $ "X"


-------------------------------------------------------------------------------
-- | New Attendance inputs.
divAttendanceNew
        :: [FeedForm]   -- ^ Form feedback
        -> [FeedEvent]  -- ^ Event edit feedback
        -> Event        -- ^ Event that the new people will be attached to.
        -> Integer      -- ^ Number of current students already added.
        -> Html

divAttendanceNew fsForm fsEvent event curStudents
 = H.div ! A.id     "event-attendance-new"
         ! A.class_ "list"
 $ H.table
 $ do   col ! A.class_ "index"
        col ! A.class_ "name"
        col ! A.class_ "actions"

        -- When the event is new then leave focus on the details rather
        -- than the attandance fields.
        let EventId eid = eventId event
        let takeFocus   = not $ eid == 0

        -- When the form has invalid details field then prevent input
        -- of more attendees.
        let hasInvalidFields
                = not $ null [x | FeedFormInvalid x _ _ <- fsForm]

        mapM_ (trNewAttendance fsEvent takeFocus hasInvalidFields curStudents)
                [0 .. 4]


-------------------------------------------------------------------------------
trNewAttendance fsEvent takeFocus disable curStudents ix
 -- Search feedback, where no match was found.
 | [names] <- [names | FeedEventSearchFoundNone ix' names <- fsEvent
                     , ix == ix' ]
 = tr
 $ do   td $ H.toMarkup (show $ curStudents + ix + 1)
        tdFeedback disable (takeFocus && ix == 0)
                names ["(no matches)"]

 -- Search feedback, where multiple matches were found.
 | [str]   <- [names  | FeedEventSearchFoundMultiString ix' names  <- fsEvent
                      , ix == ix' ]
 , psMatch <- [pMatch | FeedEventSearchFoundMultiPerson ix' pMatch <- fsEvent
                      , ix == ix' ]
 = tr
 $ do   td $ H.toMarkup (show $ curStudents + ix + 1)
        tdFeedback disable (takeFocus && ix == 0) str
         $  [ "multiple matches" ]
         ++ [ "- " ++ personDisplayName pMatch | pMatch <- take 5 $ psMatch ]
         ++ (if length psMatch >= 6 then ["..."] else [])

 -- Empty field.
 | otherwise
 = tr
 $ do   td $ H.toMarkup (show $ curStudents + ix + 1)
        tdEmpty    disable (takeFocus && ix == 0)


-------------------------------------------------------------------------------
tdEmpty disable focus
 = td $ input   ! A.type_        "text"
                ! A.name         "addPerson"
                ! A.autocomplete "off"
                !? (focus,   A.autofocus, "on")
                !? (disable, A.disabled,  "on")


tdFeedback :: Bool -> Bool -> String -> [String] -> Html
tdFeedback disable focus names ssMsg
 = td $ do
        input   ! A.type_        "text"
                ! A.name         "addPerson"
                ! A.autocomplete "off"
                ! A.value        (H.toValue names)
                !? (focus,   A.autofocus, "on")
                !? (disable, A.disabled,  "on")

        forM_ ssMsg $ \s -> do
                string s
                H.br

