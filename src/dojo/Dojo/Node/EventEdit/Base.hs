
module Dojo.Node.EventEdit.Base where
import Dojo.Node.EventEdit.Arg
import Dojo.Data.Event
import Dojo.Data.User
import Dojo.Data.Person
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Data.Set                       as Set
import Data.String
import Dojo.Framework


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

