
module Dojo.Node.EventEdit.FormAttend (formEventAttend) where
import Dojo.Node.EventEdit.Arg
import Dojo.Node.EventEdit.Base
import Dojo.Node.EventEdit.Details
import Dojo.Data.Session
import Dojo.Data.Event
import Dojo.Data.Person
import Dojo.Framework.Form
import Dojo.Framework
import Dojo.Paths
import Dojo.Chrome

import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Data.Set                       as Set


-------------------------------------------------------------------------------
-- | Produce a html form to edit details of a single event.
--    We don't allow the eventid to be edited because this is the primary
--    key for the person table.
formEventAttend :: Session -> EventForm -> Html
formEventAttend ss eform
 = H.form ! A.action (H.toValue $ eventFormPath eform)
 $ do
        let path    = eventFormPath eform
        let fsForm  = eventFormFeedForm eform

        -- Stash args from the target path as hidden fields.
        mapM_   (\(fieldName, fieldData)
                 -> H.input ! A.type_ "hidden"
                            ! A.name  (H.toValue fieldName)
                            ! A.value (H.toValue fieldData))
                (pathFields path)

        let event = eventFormEventValue eform

        -- Only bother showing site user name to admin users.
        let mUserCreatedBy
                = if sessionIsAdmin ss
                        then eventFormCreatedByUser eform
                        else Nothing

        let Just personCreatedBy
                = eventFormCreatedByPerson eform

        divEventDescription
                event mUserCreatedBy personCreatedBy

        (case eventId event of
          Nothing  -> return ()
          Just eid -> tableActions [pathEventView ss eid])

        divEventAttendance  eform
        H.input ! A.type_  "submit"
                ! A.value  "Add"

        -- Feedback about updated and invalid fields.
        htmlFeedForm fsForm niceNameOfEventField

        H.br; H.br

        -- List of regular attendees to events of this class.
        divRegulars eform


-------------------------------------------------------------------------------
divEventAttendance :: EventForm -> Html
divEventAttendance eform
 = do
        let fsForm      = eventFormFeedForm eform
        let event       = eventFormEventValue eform
        let fsEvent     = eventFormFeedEvent eform
        let psAttend    = eventFormAttendance eform

        -- List of people currently attending the event.
        divAttendanceCur eform

        -- Entry boxes to accept names of new attandees.
        let curStudents = fromIntegral $ length psAttend
        divAttendanceNew fsForm fsEvent event curStudents


-------------------------------------------------------------------------------
-- | Table of people currently listed as attending the event.
divAttendanceCur :: EventForm -> Html
divAttendanceCur eform
 = H.div ! A.id     "event-attendance-cur"
         ! A.class_ "list"
 $ H.table
 $ do   let path        = eventFormPath eform
        let fsEvent     = eventFormFeedEvent eform
        let psAttend    = eventFormAttendance eform
        let bCanDel     = eventFormAttendanceDeletable eform

        H.col ! A.class_ "index"
        H.col ! A.class_ "name"

        when bCanDel
         $ H.col ! A.class_ "actions"

        H.tr $ do H.th "#"
                  H.th "attendees"
                  when (bCanDel && (not $ null psAttend))
                   $ H.th "del" ! A.style  "text-align: center"

        -- Highlight the people that were just added.
        let pidsAdded = [pid | FeedEventPersonAdded pid <- fsEvent ]

        zipWithM_ (trCurAttendance bCanDel pidsAdded path) [1..] psAttend


-- | Row for person that is currently listed as attending the event.
trCurAttendance :: Bool -> [PersonId] -> Path -> Int -> Person -> Html
trCurAttendance bCanDel pidsAdded path ix person
 = H.tr $ do
        -- Index of person in the list.
        H.td $ H.toMarkup (show ix)

        -- If the person was just added to the list then
        -- highlight their name as feedback.
        let bJustAdded
             = case personId person of
                Nothing  -> False
                Just pid -> elem pid pidsAdded

        H.td !? (bJustAdded, A.class_, "updated")
             $ H.toMarkup
             $ maybe "(person)" pretty $ personDisplayName person

        -- Show 'X' to delete the person from the event.
        if  | Just pid <- personId person
            , bCanDel
            -> (H.td ! A.style  "text-align: center")
             $ (H.a  ! A.class_ "link"
                     ! (A.href $ H.toValue $ path <&> [("delPerson", pretty pid)]))
             $ (H.i  ! A.class_ "material-icons md-36 red")
             $ "remove_circle_outline"

            | otherwise
            -> H.td $ return ()


-------------------------------------------------------------------------------
-- | New Attendance inputs.
divAttendanceNew
        :: [FeedForm]   -- ^ Form feedback
        -> [FeedEvent]  -- ^ Event edit feedback
        -> Event        -- ^ Event that the new people will be attached to.
        -> Integer      -- ^ Number of current students already added.
        -> Html

divAttendanceNew fsForm fsEvent event curStudents
 = H.div ! A.id     "event-attendees"
         ! A.class_ "list"
 $ H.table
 $ do   H.col ! A.class_ "index"
        H.col ! A.class_ "name"
        H.col ! A.class_ "actions"

        -- When the event is new then leave focus on the details rather
        -- than the attandance fields.
        let bTakeFocus = isJust $ eventId event

        -- When the form has invalid details field then prevent input
        -- of more attendees.
        let hasInvalidFields
                = not $ null [x | FeedFormInvalid x _ _ <- fsForm]

        -- Show entry boxes for new names.
        forM_ [0 .. 2] $ \ix ->
         trNewAttendance fsEvent bTakeFocus
                hasInvalidFields curStudents ix


trNewAttendance fsEvent takeFocus disable curStudents ix
 -- Search feedback, where no match was found.
 | [names] <- [names | FeedEventSearchFoundNone ix' names <- fsEvent
                     , ix == ix' ]
 = H.tr
 $ do   H.td $ H.toMarkup (show $ curStudents + ix + 1)
        tdFeedback disable (takeFocus && ix == 0)
                names ["(unknown name)"]

 -- Search feedback, where multiple matches were found.
 | [str]   <- [names  | FeedEventSearchFoundMultiString ix' names  <- fsEvent
                      , ix == ix' ]
 , psMatch <- [pMatch | FeedEventSearchFoundMultiPerson ix' pMatch <- fsEvent
                      , ix == ix' ]
 = H.tr
 $ do   H.td $ H.toMarkup (show $ curStudents + ix + 1)
        tdFeedback disable (takeFocus && ix == 0) str
         $  [ "multiple matches" ]
         ++ [ "- " ++ maybe "(person)" pretty (personDisplayName pMatch)
            | pMatch <- take 5 $ psMatch ]
         ++ (if length psMatch >= 6 then ["..."] else [])

 -- Empty field.
 | otherwise
 = H.tr
 $ do   H.td $ H.toMarkup (show $ curStudents + ix + 1)
        tdEmpty disable (takeFocus && ix == 0)


-------------------------------------------------------------------------------
divRegulars :: EventForm -> Html
divRegulars eform
 = H.div ! A.id     "event-attendees"
         ! A.class_ "list"
 $ H.table
 $ do   H.col ! A.class_ "index"
        H.col ! A.class_ "name"
        H.col ! A.class_ "actions"

        -- Add a link to add a regular attendee that is not already listed.
        let psAttend
                = Set.fromList $ map personId
                $ eventFormAttendance eform

        let psRegular
                = take 20
                $ [ pRegular | pRegular <- eventFormRegulars eform
                             , not $ Set.member (personId pRegular) psAttend]

        H.tr $ do
                H.th ""
                H.th "regulars"
                if not $ null psRegular
                 then H.th ! A.style "text-align: center" $ "add"
                 else H.th ""

        forM_ psRegular (trNewRegular eform)

trNewRegular eform person
 = H.tr $ do
        let Just pid  = personId person
        let pathAdd   = eventFormPath eform <&> [("addPerson", pretty pid)]

        -- Empty column to align with attendee index column
        -- in previous table.
        H.td $ return ()

        H.td $ (H.a ! A.href (H.toValue pathAdd))
           $ H.toMarkup
                $ maybe "(person)" pretty
                $ personDisplayName person

        -- Show '+' to add the person to the event.
        H.td  ! A.style "text-align:center"
            $ (H.a ! A.href (H.toValue pathAdd))
            $ (H.i ! A.class_ "material-icons md-36 green")
            $ "add_circle_outline"


-------------------------------------------------------------------------------
tdEmpty disable focus
 = H.td $ H.input
        ! A.type_        "text"
        ! A.name         "addName"
        ! A.autocomplete "off"
        !? (focus,   A.autofocus, "on")
        !? (disable, A.disabled,  "on")


tdFeedback :: Bool -> Bool -> String -> [String] -> Html
tdFeedback disable focus names ssMsg
 = H.td $ do
        H.input ! A.type_        "text"
                ! A.name         "addName"
                ! A.autocomplete "off"
                ! A.value        (H.toValue names)
                !? (focus,   A.autofocus, "on")
                !? (disable, A.disabled,  "on")

        forM_ ssMsg $ \s -> do
                H.string s
                H.br