
module Dojo.Node.PersonAdd where
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


-- | Add a new person, using a vertical form.
cgiPersonAdd
        :: Session
        -> [(String, String)]
        -> CGI CGIResult

cgiPersonAdd ss inputs
 = do
        -- Parse the arguments.
        let Just args
                = sequence
                $ map argOfKeyVal inputs

        case lookup "FirstName" inputs of
         Just firstName
           -> cgiPersonAdd_insert ss args inputs firstName

         _ -> cgiPersonAdd_entry  ss args


-- If we don't have the first name then show the entry form.
cgiPersonAdd_entry ss args
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "Add Person"
        pageBody
         $ do   tablePaths $ pathsJump ss
                formPerson args (pathPersonAdd ss) (zeroPerson "(required)")


-- If we have at least the first name then we can add it to the database.
cgiPersonAdd_insert ss _args inputs firstName
 = case loadPerson inputs (zeroPerson firstName) of

    -- All the fields parsed.
    Right person'
     -> do
        -- Connect to the database.
        conn    <- liftIO $ connectSqlite3 databasePath

        -- Insert the new person.
        liftIO $ insertPerson conn person'
        liftIO $ commit conn
        liftIO $ disconnect conn

        -- Go back to the person list.
        redirect $ flatten $ pathPersonList ss

