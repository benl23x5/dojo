
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
formEvent :: [FeedForm] -> Path -> Event -> [Person] -> Html
formEvent fsFeed path event attendance
 = form ! A.action (H.toValue path)
 $ do
        -- Stash args from the target path as hidden fields.
        mapM_   (\(fieldName, fieldData)
                 -> input ! A.type_ "hidden"
                          ! A.name  (H.toValue fieldName)
                          ! A.value (H.toValue fieldData))
                (pathFields path)

        -- Feedback about updated and invalid fields.
        htmlFeedForm fsFeed niceNameOfEventField

        -- Event details.
        divEventDetails    fsFeed event
        divEventAttendance fsFeed [] path event attendance
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
        H.table $ trInput fsFeed
         "Date" "date (dd-mm-yyyy)"
         (pretty $ eventDate event) (Just "(required)")

        H.table $ trInput fsFeed
         "Time" "time (hh:mm 24hr)"
         (pretty $ eventTime event) (Just "(required)")

        -- When this is a new event put focus on the location input field,
        -- otherwise allow focus to be taken by the last person entry field.
        let EventId eid = eventId event
        let takeFocus   = eid == 0

        if | takeFocus
           -> H.table $ trInputWithFocus fsFeed
                 "Location" "location"
                 (pretty $ eventLocation event) (Just "(required)")

           | otherwise
           -> H.table $ trInput fsFeed
                 "Location" "location"
                 (pretty $ eventLocation event) (Just "(required)")

        H.table $ trInput fsFeed
         "Type" "type (dojo, ttc)"
         (pretty $ eventType event) (Just "(required)")


-------------------------------------------------------------------------------
divEventAttendance :: [FeedForm] -> [Arg] -> Path -> Event -> [Person] -> Html
divEventAttendance fsFeed _args path event attendance
 = do
        -- TODO: fix people added feedback
        divAttendanceCur [] path attendance

        let curStudents = fromIntegral $ length attendance
        divAttendanceNew fsFeed [] event curStudents


-------------------------------------------------------------------------------
-- | Table of people currently listed as attending the event.
divAttendanceCur args path attendance
 = H.div ! A.id     "event-attendance-cur"
         ! A.class_ "list"
 $ H.table
 $ do   col' "index"
        col' "name"
        col' "actions"

        tr $ do th "#"
                th "attendees"
                th "del"

        -- Highlight the people that were just added.
        let pidsAdded = [pid | ArgPersonAdded pid <- args ]

        zipWithM_ (trCurAttendance pidsAdded path) [1..] attendance

 where  col' c  = col ! A.class_ c


-- | Row for person that is currently listed as attending the event.
trCurAttendance :: [PersonId] -> Path -> Int -> Person -> Html
trCurAttendance pidsAdded path ix person
 = tr
 $ do   td $ H.toMarkup (show ix)

        td !? (elem (personId person) pidsAdded, A.class_, "updated")
           $ H.toMarkup
           $ personDisplayName person

        td $ H.a ! A.class_ "link"
                 ! (A.href $ H.toValue
                           $ path <&> [("delPerson", pretty $ personId person)])
                 $ "X"


-------------------------------------------------------------------------------
-- | New Attendance inputs.
divAttendanceNew
        :: [FeedForm]   -- ^ Form feedback
        -> [Arg]        -- ^ Pass back args to we can display search feedback
                        --   In the entry fields for terms that didn't resolve
                        --   to a single person.
        -> Event        -- ^ Event that the new people will be attached to.
        -> Integer      -- ^ Number of current students already added.
        -> Html

divAttendanceNew fsFeed args event curStudents
 = H.div ! A.id     "event-attendance-new"
         ! A.class_ "list"
 $ H.table
 $ do   col' "index"
        col' "name"
        col' "actions"

        -- When the event is new then leave focus on the details rather
        -- than the attandance fields.
        let EventId eid = eventId event
        let takeFocus   = not $ eid == 0

        -- When the form has invalid details field then prevent input
        -- of more attendees.
        let hasInvalidFields
                = not $ null [x | FeedFormInvalid x _ _ <- fsFeed]

        mapM_ (trNewAttendance args takeFocus hasInvalidFields curStudents)
                [0 .. 4]

 where  col' c  = col ! A.class_ c


trNewAttendance args takeFocus disable curStudents ix
 -- Search feedback, where no match was found.
 | [names]      <- [names | ArgSearchFoundNone ix' names <- args
                          , ix == ix' ]
 = tr
 $ do   td $ H.toMarkup (show $ curStudents + ix + 1)
        tdFeedback disable (takeFocus && ix == 0) names "(no matches)"

 -- Search feedback, where multiple matches were found.
 | [str]        <- [names | ArgSearchFoundMultiString ix' names <- args
                          , ix == ix' ]
 = tr
 $ do   td $ H.toMarkup (show $ curStudents + ix + 1)
        tdFeedback disable (takeFocus && ix == 0) str   "(multiple matches)"

 -- Empty field.
 | otherwise
 = tr
 $ do   td $ H.toMarkup (show $ curStudents + ix + 1)
        tdEmpty    disable (takeFocus && ix == 0)


tdEmpty disable focus
 = td $ input   ! A.type_        "text"
                ! A.name         "addPerson"
                ! A.autocomplete "off"
                !? (focus,   A.autofocus, "on")
                !? (disable, A.disabled,  "on")


tdFeedback disable focus names msg
 = td $ do
        input   ! A.type_        "text"
                ! A.name         "addPerson"
                ! A.autocomplete "off"
                ! A.value        (H.toValue names)
                !? (focus,   A.autofocus, "on")
                !? (disable, A.disabled,  "on")
        msg

