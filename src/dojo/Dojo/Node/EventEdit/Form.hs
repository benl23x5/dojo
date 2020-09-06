
module Dojo.Node.EventEdit.Form (formEvent, EventForm(..)) where
import Dojo.Node.EventEdit.Arg
import Dojo.Data.Event
import Dojo.Data.User
import Dojo.Data.Person
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Data.Set                       as Set
import Data.String


-------------------------------------------------------------------------------
-- | Specification for the event entry form.
data EventForm
        = EventForm
        { -- | URL to the form itself.
          eventFormPath                 :: Path

          -- | UI feedback for the overall form.
        , eventFormFeedForm             :: [FeedForm]

          -- | UI feedback for the event details.
        , eventFormFeedEvent            :: [FeedEvent]

          -- | The original devent details to show in the form.
        , eventFormEventValue           :: Event

          -- | List of available event types to show in dropdown selector.
        , eventFormEventTypes           :: [EventType]

          -- | People currently listed as attending the event.
        , eventFormAttendance           :: [Person]

          -- | Regular attendees for events of this class.
        , eventFormRegulars             :: [Person]

          -- | List of available dojos to show in dojo dropdown selector.
        , eventFormDojosAvail           :: [PersonDojo]

          -- | Whether to display edit control for date, time, loc, type etc.
          --   or just the natural language summary.
        , eventFormDetailsEditable      :: Bool

          -- | Whether to show controls for deleting names from the list.
        , eventFormAttendanceDeletable  :: Bool

          -- | User details of who created the form.
        , eventFormCreatedByUser        :: Maybe User

          -- | Person details of who created the form.
        , eventFormCreatedByPerson      :: Maybe Person }
        deriving Show


-------------------------------------------------------------------------------
-- | Produce a html form to edit tails of a single event.
--    We don't allow the eventid to be edited because this is the primary
--    key for the person table.
formEvent :: EventForm -> Html
formEvent eform
 = form ! A.action (H.toValue $ eventFormPath eform)
 $ do
        let path        = eventFormPath eform
        let fsForm      = eventFormFeedForm eform

        -- Stash args from the target path as hidden fields.
        mapM_   (\(fieldName, fieldData)
                 -> input ! A.type_ "hidden"
                          ! A.name  (H.toValue fieldName)
                          ! A.value (H.toValue fieldData))
                (pathFields path)

        -- Feedback about updated and invalid fields.
        htmlFeedForm fsForm niceNameOfEventField

        -- Event details.
        (if eventFormDetailsEditable eform
          then divEventEditDetails eform
          else divEventShowDetails eform)

        divEventAttendance  eform
        H.br

        -- Save button.
        input   ! A.type_  "submit"
                ! A.class_ "buttonFull"
                ! A.value  "Save"


-------------------------------------------------------------------------------
divEventShowDetails :: EventForm -> Html
divEventShowDetails eform
 = H.div ! A.id "event-details-edit" ! A.class_ "details"
 $ do
        let event       = eventFormEventValue eform
        let mpCreated   = eventFormCreatedByPerson eform
        let muCreated   = eventFormCreatedByUser eform

        H.table $ do
         tr $ td $ H.string
            $ maybe "[sometype]" (\v -> pretty v ++ " class") (eventType event)
            ++ " by "
            ++ (fromMaybe "[someperson]"
                (join $ fmap personDisplayName mpCreated))
            ++ " ("
            ++ (maybe "[someuser]"   (pretty . userName) muCreated)
            ++ ")."

         tr $ td $ H.string
            $  maybe "[somewhere]" pretty  (eventLocation event)
            ++ maybe "[someday]"  (\v -> " on " ++ pretty v)  (eventDate event)
            ++ maybe "[sometime]" (\v -> " at " ++ pretty v)  (eventTime event)
            ++ "."


-------------------------------------------------------------------------------
divEventEditDetails :: EventForm  -> Html
divEventEditDetails eform
 = H.div ! A.id "event-details-edit" ! A.class_ "details"
 $ do
        let fsForm      = eventFormFeedForm eform
        let event       = eventFormEventValue eform
        let eventTypes  = eventFormEventTypes eform
        let dojos       = eventFormDojosAvail eform

        tableFields fsForm
         [ ( "Date", "date (dd-mm-yyyy)"
           , maybe "" pretty $ eventDate event
           , Just "(required)"
           , False)

         , ( "Time", "time (hh:mm 24hr)"
           , maybe "" pretty $ eventTime event
           , Just "(required)"
           , False)
         ]

        -- When this is a new event put focus on the location input field,
        -- otherwise allow focus to be taken by the last person entry field.
        --  TODO:  reinstate focus on location when eid == 0
        --  let EventId eid = eventId event
        let sDojo = maybe "" pretty $ eventLocation event
        let sType = maybe "" pretty $ eventType event
        H.table
         $ do   col ! A.class_ "Col2A"
                col ! A.class_ "Col2B"
                tr $ do th "location"; th "type"
                tr $ do
                        td $ (H.select ! A.name "Location")
                         $ do   H.option ! A.value "" $ "(unspecified)"
                                forM_ (map pretty dojos) (optSelected sDojo)

                        td $ (H.select ! A.name "Type")
                         $ do   H.option ! A.value "" $ "(unspecified)"
                                forM_ (map pretty eventTypes) (optSelected sType)

 where  optSelected sSel sVal
         = (H.option
                !  A.value (fromString sVal)
                !? (sSel == sVal, A.selected, "true"))
                (H.toHtml sVal)

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

        -- List of regular attendees to events of this class.
        divRegulars eform


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

        col ! A.class_ "index"
        col ! A.class_ "name"

        when bCanDel
         $ col ! A.class_ "actions"

        tr $ do th "#"
                th "current attendees"
                when bCanDel $ th "del"

        -- Highlight the people that were just added.
        let pidsAdded = [pid | FeedEventPersonAdded pid <- fsEvent ]

        zipWithM_ (trCurAttendance bCanDel pidsAdded path) [1..] psAttend


-- | Row for person that is currently listed as attending the event.
trCurAttendance :: Bool -> [PersonId] -> Path -> Int -> Person -> Html
trCurAttendance bCanDel pidsAdded path ix person
 = tr $ do
        -- Index of person in the list.
        td $ H.toMarkup (show ix)

        -- If the person was just added to the list then
        -- highlight their name as feedback.
        let bJustAdded
             = case personId person of
                Nothing  -> False
                Just pid -> elem pid pidsAdded

        td !? (bJustAdded, A.class_, "updated")
           $ H.toMarkup
           $ maybe "(person)" pretty $ personDisplayName person

        -- Show 'X' to delete the person from the event.
        if  | Just pid <- personId person
            , bCanDel
            -> td $ H.a ! A.class_ "link"
                        ! (A.href $ H.toValue
                                  $ path <&> [("delPerson", pretty pid)])
                        $ "X"
            | otherwise
            -> td $ return ()


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
 $ do   col ! A.class_ "index"
        col ! A.class_ "name"
        col ! A.class_ "actions"

        -- When the event is new then leave focus on the details rather
        -- than the attandance fields.
        let bTakeFocus = isJust $ eventId event

        -- When the form has invalid details field then prevent input
        -- of more attendees.
        let hasInvalidFields
                = not $ null [x | FeedFormInvalid x _ _ <- fsForm]

        -- Show entry boxes for new names.
        forM_ [0 .. 1] $ \ix ->
         trNewAttendance fsEvent bTakeFocus
                hasInvalidFields curStudents ix


trNewAttendance fsEvent takeFocus disable curStudents ix
 -- Search feedback, where no match was found.
 | [names] <- [names | FeedEventSearchFoundNone ix' names <- fsEvent
                     , ix == ix' ]
 = tr
 $ do   td $ H.toMarkup (show $ curStudents + ix + 1)
        tdFeedback disable (takeFocus && ix == 0)
                names ["(unknown name)"]

 -- Search feedback, where multiple matches were found.
 | [str]   <- [names  | FeedEventSearchFoundMultiString ix' names  <- fsEvent
                      , ix == ix' ]
 , psMatch <- [pMatch | FeedEventSearchFoundMultiPerson ix' pMatch <- fsEvent
                      , ix == ix' ]
 = tr
 $ do   td $ H.toMarkup (show $ curStudents + ix + 1)
        tdFeedback disable (takeFocus && ix == 0) str
         $  [ "multiple matches" ]
         ++ [ "- " ++ maybe "(person)" pretty (personDisplayName pMatch)
            | pMatch <- take 5 $ psMatch ]
         ++ (if length psMatch >= 6 then ["..."] else [])

 -- Empty field.
 | otherwise
 = tr
 $ do   td $ H.toMarkup (show $ curStudents + ix + 1)
        tdEmpty disable (takeFocus && ix == 0)


-------------------------------------------------------------------------------
divRegulars :: EventForm -> Html
divRegulars eform
 = H.div ! A.id     "event-attendees"
         ! A.class_ "list"
 $ H.table
 $ do   col ! A.class_ "index"
        col ! A.class_ "name"
        col ! A.class_ "actions"

        tr $ do th ""
                th "regular attendees"
                th "add"

        -- Add a link to add a regular attendee that is not already listed.
        let psAttend
                = Set.fromList $ map personId
                $ eventFormAttendance eform

        let psRegular
                = take 10
                $ [ pRegular | pRegular <- eventFormRegulars eform
                             , not $ Set.member (personId pRegular) psAttend]

        forM_ psRegular (trNewRegular eform)

trNewRegular eform person
 = tr $ do
        let path = eventFormPath eform

        td $ return ()
        td $ H.toMarkup
                $ maybe "(person)" pretty
                $ personDisplayName person

        -- Show 'X' to delete the person from the event.
        if  | Just pid <- personId person
            -> td $ H.a ! A.class_ "link"
                        ! (A.href $ H.toValue
                                  $ path <&> [("addPerson", pretty pid)])
                        $ "^"
            | otherwise
            -> td $ return ()


-------------------------------------------------------------------------------
tdEmpty disable focus
 = td $ input   ! A.type_        "text"
                ! A.name         "addName"
                ! A.autocomplete "off"
                !? (focus,   A.autofocus, "on")
                !? (disable, A.disabled,  "on")


tdFeedback :: Bool -> Bool -> String -> [String] -> Html
tdFeedback disable focus names ssMsg
 = td $ do
        input   ! A.type_        "text"
                ! A.name         "addName"
                ! A.autocomplete "off"
                ! A.value        (H.toValue names)
                !? (focus,   A.autofocus, "on")
                !? (disable, A.disabled,  "on")

        forM_ ssMsg $ \s -> do
                string s
                H.br

