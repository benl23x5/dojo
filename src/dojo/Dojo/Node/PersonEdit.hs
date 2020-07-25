
module Dojo.Node.PersonEdit (cgiPersonEdit) where
import Dojo.Node.PersonEdit.Form
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.Dojo
import Dojo.Paths
import Dojo.Fail
import Dojo.Chrome
import Dojo.Framework
import Config
import qualified Text.Blaze.Html5               as H


-------------------------------------------------------------------------------
-- | Edit a single person, using a vertical form.
--      ?pe [&pid=INT] [... &p=FIELDNAME ...]
--      Edit an existing person, with an id and list of updated fields.
--      When there is no pid argument then we add a new person.
--
--      The 'p'/updated fields are passed if a previous action just updated
--      the event record. Display UI feedback for these.
--
cgiPersonEdit :: Session -> [(String, String)] -> CGI CGIResult
cgiPersonEdit ss inputs
 = do
        conn            <- liftIO $ connectSqlite3 databasePath
        dojos           <- liftIO $ getDojos conn
        memberLevels    <- liftIO $ getMembershipLevels conn

        -- Fields of the user entry to update.
        let fieldUpdates
                = [ (field, value)
                  | (field@(f : _), value) <- inputs
                  , isUpper f ]

        -- See if we were given an existing person id.
        (mpid, person)
         <- case lookup "pid" inputs of
             Just strPersonId
              -> case parse strPersonId of
                  Left err
                   -> throw $ FailParse "person id" strPersonId err

                  Right pid
                   -> do person  <- liftIO $ getPerson conn pid
                         return (personId person, person)

             Nothing
              -> do -- First name starts off empty, and we won't be able
                    -- to add the record to the db until it's filled.
                    let person = zeroPerson ""
                    return (Nothing, person)

        if
         -- We were given new details as args to the path,
         --  so update the existing details and show the form again.
         | (not $ null fieldUpdates)
         -> cgiPersonEdit_update
                ss conn mpid person dojos memberLevels fieldUpdates

         -- We have no updates yet,
         --  so show the form and wait for entry.
         | otherwise
         -> do  liftIO $ disconnect conn
                cgiPersonEdit_entry ss person dojos memberLevels


-------------------------------------------------------------------------------
-- | We haven't got any updates yet,
--    so show the entry form.
cgiPersonEdit_entry
        :: Session
        -> Person                   -- Person to edit.
        -> [PersonDojo]             -- Available dojos.
        -> [PersonMembershipLevel]  -- Available membership levels.
        -> CGI CGIResult

cgiPersonEdit_entry ss person dojos memberLevels
 = outputFPS $ renderHtml
 $ htmlPersonEdit ss person dojos memberLevels []


-------------------------------------------------------------------------------
-- We got some updates.
--  Update the database and show the updated form.
cgiPersonEdit_update
        :: IConnection conn
        => Session -> conn
        -> Maybe PersonId           -- Person id if we have an existing one.
        -> Person                   -- Person to edit.
        -> [PersonDojo]             -- Available dojos.
        -> [PersonMembershipLevel]  -- Available membership levels.
        -> [(String, String)]       -- Fields to update.
        -> CGI CGIResult

cgiPersonEdit_update ss conn
        Nothing person dojos memberLevels updates
 = do   person' <- liftIO $ insertPerson conn person
        cgiPersonEdit_update ss conn
                (personId person') person' dojos memberLevels
                updates

cgiPersonEdit_update ss conn
        (Just _pid) personOld dojos memberLevels updates
 = case loadPerson updates personOld of
    -- Some fields didn't parse.
    Left fieldErrors
     -> do liftIO $ disconnect conn
           let fsFeed =
                [ FeedFormInvalid sField sValue sError
                | (sField, sValue, ParseError sError) <- fieldErrors ]

           outputFPS $ renderHtml
             $ htmlPersonEdit ss personOld dojos memberLevels fsFeed

    -- All the fields parsed.
    Right personNew
      -> do liftIO $ updatePerson conn personNew
            liftIO $ commit conn
            liftIO $ disconnect conn

            let fsFeed =
                 [ FeedFormUpdated sField
                 | sField <- diffPerson personOld personNew ]

            outputFPS $ renderHtml
             $ htmlPersonEdit ss personNew dojos memberLevels fsFeed


-------------------------------------------------------------------------------
-- | Html for person edit page.
htmlPersonEdit
        :: Session
        -> Person -> [PersonDojo] -> [PersonMembershipLevel]
        -> [FeedForm] -> Html

htmlPersonEdit ss person dojos memberLevels fsFeed
 = H.docTypeHtml
 $ do   pageHeader "Editing Person"
        pageBody
         $ do   tablePaths $ pathsJump ss

                tablePaths
                 $ case personId person of
                    Nothing     -> []
                    Just pid    -> [pathPersonView ss pid]

                formPerson fsFeed
                        (pathPersonEdit ss (personId person))
                        person dojos memberLevels
