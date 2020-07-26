
module Dojo.Data.Attendance where
import Dojo.Base
import Dojo.Data.Person
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-- | Whether a person attended a list of events.
data Attendance
        = Attendance
        { attendancePerson      :: Person
        , attendanceFlags       :: [Bool] }


-- | Render an attendance record as a row of checkboxes.
trAttendance :: Attendance -> Html
trAttendance (Attendance person flags)
 = tr
 $ do   tdPerson person
        mapM_ tdFlag flags

 where  -- If we've lost the name due to an internal bug then still
        --  display a '(person)' placeholder so we can click on the row.
        tdPerson person'
         = td   $ toMarkup
                $ fromMaybe "(person)" $ personDisplayName person'

        tdFlag _flag
         = td   $ H.input
                        ! A.type_ "checkbox"
                        ! A.name  "check1"
