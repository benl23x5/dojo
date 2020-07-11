
module Dojo.Node.PersonEdit where
import Dojo.Node.PersonEdit.Form
import Dojo.Node.PersonEdit.Arg
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Paths
import Dojo.Chrome
import Dojo.Framework
import Dojo.Base
import Config
import qualified Text.Blaze.Html5               as H


-------------------------------------------------------------------------------
-- | Edit a single person, using a vertical form.
--      ?pEdit [&pid=INT] [... &p=FIELDNAME ...]
--      Edit an existing person, with an id and list of updated fields.
--      When there is no pid argument then we add a new person.
--
--      The 'p'/updated fields are passed if a previous action just updated
--      the event record. Display UI feedback for these.
--
cgiPersonEdit :: Session -> [(String, String)] -> CGI CGIResult
cgiPersonEdit ss inputs
 = do
        -- TODO: check incoming arguments.
        let Just _args
                = sequence
                $ map argOfKeyVal inputs

        -- Field of the user entry to update.
        let fieldUpdates
                = [ (field, value)
                  | (field@(f : _), value) <- inputs
                  , isUpper f ]

        -- Connect to the database.
        conn    <- liftIO $ connectSqlite3 databasePath

        -- See if we were given an existing person id.
        (mpid, person)
         <- case lookup "pid" inputs of
             -- Try to load existing person record.
             Just strPersonId
              -> do -- TODO: better parsing.
                    let Right pid = parse strPersonId
                    Just person  <- liftIO $ getPerson conn pid
                    return (personId person, person)

             Nothing
              -> do let person = zeroPerson ""
                    return (Nothing, person)

        if
         -- Update person details.
         | (not $ null fieldUpdates)
         -> cgiPersonEdit_update
                ss conn mpid person fieldUpdates

         -- Show the form and wait for entry.
         | otherwise
         -> do  liftIO $ disconnect conn
                cgiPersonEdit_entry ss person


-------------------------------------------------------------------------------
-- | We haven't got any updates yet, so show the entry form.
cgiPersonEdit_entry ss person
 = outputFPS $ renderHtml
 $ htmlPersonEdit ss person []


-------------------------------------------------------------------------------
-- We got some updates.
--  Update the database and show the updated form.
cgiPersonEdit_update
        :: IConnection conn
        => Session -> conn
        -> Maybe PersonId       -- person id if we are editing an existing person
        -> Person               -- person
        -> [(String, String)]   -- fields to update
        -> CGI CGIResult

cgiPersonEdit_update ss conn
        Nothing person updates
 = do   person' <- liftIO $ insertPerson conn person
        cgiPersonEdit_update ss conn
                (personId person') person'
                updates

cgiPersonEdit_update ss conn
        (Just _pid) personOld updates
 = case loadPerson updates personOld of
    -- Some fields didn't parse.
    Left fieldErrors
     -> do let fsFeed =
                [ FeedFormInvalid sField sValue sError
                | (sField, sValue, ParseError sError) <- fieldErrors ]

           outputFPS $ renderHtml
             $ htmlPersonEdit ss personOld fsFeed

    -- All the fields parsed.
    Right personNew
      -> do liftIO $ updatePerson conn personNew
            liftIO $ commit conn
            liftIO $ disconnect conn

            let fsFeed =
                 [ FeedFormUpdated sField
                 | sField <- diffPerson personOld personNew ]

            outputFPS $ renderHtml
             $ htmlPersonEdit ss personNew fsFeed


-------------------------------------------------------------------------------
-- | Html for person edit page.
htmlPersonEdit :: Session -> Person -> [FeedForm] -> Html
htmlPersonEdit ss person fsFeed
 = H.docTypeHtml
 $ do   pageHeader "Editing Person"
        pageBody
         $ do   tablePaths $ pathsJump ss

                -- TODO: different link if no person id yet.
                tablePaths
                 $ case personId person of
                    Nothing     -> []
                    Just pid    -> [pathPersonView ss pid]

                formPerson fsFeed
                        (pathPersonEdit ss (personId person))
                        person


