
module Dojo.Node.PersonList (cgiPersonList) where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Framework
import Dojo.Paths
import Dojo.Chrome
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | Table of all people with links to the per-person pages.
cgiPersonList :: Session -> [(String, String)] -> CGI CGIResult
cgiPersonList ss _inputs
 = do   conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        people  <- liftIO $ getPeople conn
        liftIO $ disconnect conn

        cgiPageNavi ss "People" "People" (pathsJump ss)
         $ do   tableActions [pathPersonAdd ss]
                divPersonList ss people


-------------------------------------------------------------------------------
-- | Build the person table.
divPersonList :: Session -> [Person] -> H.Html
divPersonList ss people
 = H.div ! A.class_ "list person-list"
 $ H.table
 $ do
        col' "ShortName"; col' "FamilyName"; col' "PhoneMobile"
        H.tr $ do H.th "pref / first"; H.th "family"; H.th "phone"

         -- Clicking on any column goes to person view page.
        forM_ people $ \person -> H.tr $ do
         -- If we've lost the name due to an internal bug then still
         --  display a '(person)' placeholder so we can click on the row.
         td' person
          $ Just $ fromMaybe "(person)"
          $ personShortName person

         td' person $ personFamilyName person

         -- Prefer showing the mobile number if we have one.
         td' person
          $ if |  Just mobile <- personPhoneMobile person
               -> Just mobile

               |  otherwise
               -> personPhoneFixed person

 where  col' c = H.col ! A.class_ c

        td' person val
         | Just pid <- personId person
         = H.td $ (H.a ! A.href (H.toValue $ pathPersonView ss pid))
                  (H.toMarkup $ fromMaybe "" $ fmap pretty val)

         | otherwise
         = H.td $ (H.toMarkup $ fromMaybe "" $ fmap pretty val)
