
module Dojo.Node.PersonList (cgiPersonList) where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Framework
import Dojo.Paths
import Dojo.Chrome
import Config
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | Table of all people with links to the per-person pages.
--      no args.
cgiPersonList
        :: Session -> [(String, String)]
        -> CGI CGIResult

cgiPersonList sid _inputs
 = do   conn    <- liftIO $ connectSqlite3 databasePath
        people  <- liftIO $ getPeople conn
        liftIO $ disconnect conn
        cgiPersonList_list sid people


cgiPersonList_list ss people
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader "People"
        pageBody
         $ do   tablePaths $ pathsJump ss
                tablePaths $ [pathPersonAdd ss]
                divPersonList ss people


-------------------------------------------------------------------------------
-- | Build the person table.
divPersonList :: Session -> [Person] -> H.Html
divPersonList ss people
 = H.div ! A.class_ "list person-list"
 $ H.table
 $ do
        col' "ShortName"; col' "FamilyName"; col' "PhoneMobile"
        tr $ do th "pref / first"; th "family"; th "phone"

         -- Clicking on any column goes to person view page.
        forM_ people $ \person -> tr $ do
         td' person $ Just $ personShortName person
         td' person $ personFamilyName person

         -- Prefer showing the mobile number if we have one.
         td' person
          $ if |  Just mobile <- personPhoneMobile person
               -> Just mobile

               |  otherwise
               -> personPhoneFixed person

 where  col' c = col ! A.class_ c

        td' person val
         | Just pid <- personId person
         = td $ (a ! A.href (H.toValue $ pathPersonView ss pid))
                (H.toMarkup $ fromMaybe "" $ fmap pretty val)

         | otherwise
         = td $ (H.toMarkup $ fromMaybe "" $ fmap pretty val)
