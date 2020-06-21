
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


-- | Edit a single person, using a vertical form.
--      ?pEdit [&pid=INT] [... &p=FIELDNAME ...]
--      Edit an existing person, with an id and list of updated fields.
--      When there is no pid argument then we add a new person.
--
--      The 'p'/updated fields are passed if a previous action just updated
--      the event record. Display UI feedback for these.
--
cgiPersonEdit
        :: Session
        -> [(String, String)]
        -> CGI CGIResult

cgiPersonEdit ss inputs
 = do
        -- Normalize incoming arguments.
        let Just args
                = sequence
                $ map argOfKeyVal inputs

        -- Field of the user entry to update.
        let fieldUpdates
                = [ (field, value)      | (field@(f : _), value) <- inputs
                                        , isUpper f ]

        -- Connect to the database.
        conn    <- liftIO $ connectSqlite3 databasePath

        -- If we have an existing personId then load the existing person data,
        --   otherwise start with an empty person record.
        (mpid, person)
         <- case lookup "pid" inputs of
             Just strPersonId
              -> do     let (Right pid) = parse strPersonId
                        person  <- liftIO $ getPerson conn pid
                        return  (Just pid, person)

             Nothing
              -> do     let person = zeroPerson "(required)"
                        return   (Nothing, person)

        if      -- Update person details.
                | not $ null fieldUpdates
                -> cgiPersonEdit_update  ss conn mpid person inputs

                -- Show the form and wait for entry.
                | otherwise
                -> do   liftIO $ disconnect conn
                        cgiPersonEdit_entry ss args mpid person


-------------------------------------------------------------------------------
-- We got some updates.
--  Update the database and show the updated form.
cgiPersonEdit_update ss conn mPid person inputs
 = case loadPerson inputs person of
    -- Some of the fields didn't parse.
    Left fieldErrs
     | Just pid <- mPid
     -> redirect $ flatten
      $ pathPersonEdit ss (Just pid)
                <&> map keyValOfArg
                        [ ArgDetailsInvalid name str
                        | (name, str, _) <- fieldErrs ]


    -- All the fields parsed, insert new person.
     | Nothing  <- mPid
     -> do
        -- Insert the new person.
        liftIO $ insertPerson conn person
        liftIO $ commit conn

        liftIO $ disconnect conn

        -- Say
        redirect $ flatten $ pathPersonList ss


    -- All the fields parsed, update existing person.
    Right person'
     | Just pid <- mPid
     -> do
        -- See if anything has changed.
        let diffFields  = diffPerson person person'

        -- Write the changes to the database.
        _       <- liftIO $ updatePerson conn person'
        liftIO $ commit conn
        liftIO $ disconnect conn

        -- Stay on the same page, but show what fields were updated.
        redirect $ flatten
         $ pathPersonEdit ss (Just pid)
                <&> map keyValOfArg (map ArgDetailsUpdated diffFields)


-------------------------------------------------------------------------------
-- We haven't got any updates yet, so show the entry form.
cgiPersonEdit_entry ss args mpid person
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader $ "Editing Person"
        pageBody
         $ do   tablePaths $ pathsJump ss
                tablePaths [pathPersonView ss $ personId person]
                formPerson args (pathPersonEdit ss mpid) person

