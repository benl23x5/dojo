
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


-- Forms ----------------------------------------------------------------------
-- | Form to change the details of a single event.
--      We don't allow the eventid to be edited because this is the primary
--      key for the person table.
formEvent :: [Arg] -> Path -> Event -> [Person] -> Html
formEvent args path event attendance
 = form ! A.action (H.toValue path)
 $ do
        -- Stash args from the target path as hidden fields.
        mapM_   (\(fieldName, fieldData)
                 -> input ! A.type_ "hidden"
                          ! A.name  (H.toValue fieldName)
                          ! A.value (H.toValue fieldData))
                (pathFields path)

        -- Event details.
        divEventDetails    args event

        -- Which people attended the event.
        divEventAttendance args path event attendance

        -- Save button,
        --  with feedback on which fields were updated next to it.
        table
         $ tr $ td
         $ do   input   ! A.type_ "submit"
                        ! A.value "Save"

                let updatedFields    = [field | ArgDetailsUpdated field <- args]
                when (not $ null updatedFields)
                 $ H.span ! A.class_ "updated"
                 $ H.toMarkup
                 $ " Updated: "
                        ++ intercalate ", "
                                 ( map (\(Just n) -> n)
                                 $ map niceNameOfEventField updatedFields)
                        ++ "."


-- Event Details --------------------------------------------------------------
divEventDetails :: [Arg] -> Event -> Html
divEventDetails args event
 = do   H.div ! A.id "event-details-edit" $ table
         $ do   tr $ do th' "Location"; th' "Type"

                -- When this is a new event put focus non the first details field.
                let EventId eid = eventId event
                let takeFocus   = eid == 0

                tr $ do td' takeFocus "Location" (pretty $ eventLocation event)
                        td' False     "Type"     (pretty $ eventType event)

        H.div ! A.id "event-details-edit" $ table
         $ do   tr $ do th' "EventId"; th' "Date"; th' "Time"
                tr $ do td $ H.toMarkup $ pretty $ eventId event
                        td' False     "Date"     (pretty $ eventDate event)
                        td' False     "Time"     (pretty $ eventTime event)


 where  -- Feedback which fields were just updated.
        updateds = mapMaybe takeDetailsUpdated args

        -- Feedback which fields have invalid values.
        invalids = mapMaybe takeDetailsInvalid args

        th' fieldName
         = let  Just niceName = niceNameOfEventField fieldName
           in   thInputFeedback updateds invalids fieldName niceName

        td' focus fieldName val
         =      tdInputFeedback focus invalids fieldName val


-- Event Attendance -----------------------------------------------------------
divEventAttendance :: [Arg] -> Path -> Event -> [Person] -> Html
divEventAttendance args path event attendance
 = do
        divAttendanceCur args path attendance

        let curStudents = fromIntegral $ length attendance
        divAttendanceNew args event curStudents


-- Current Attendance Field ---------------------------------------------------
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


-- New Attendance Field -------------------------------------------------------
divAttendanceNew
        :: [Arg]        -- ^ Pass back args to we can display search feedback
                        --   In the entry fields for terms that didn't resolve
                        --   to a single person.
        -> Event        -- ^ Event that the new people will be attached to.
        -> Integer      -- ^ Number of current students already added.
        -> Html

divAttendanceNew args event curStudents
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
                = not $ null [x | ArgDetailsInvalid x _ <- args]

        mapM_ (trNewAttendance args takeFocus hasInvalidFields curStudents)
                [0 .. 9]

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

