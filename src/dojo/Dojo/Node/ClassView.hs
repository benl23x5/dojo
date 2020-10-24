
module Dojo.Node.ClassView
        ( cgiClassView
        , trClassSummary
        , divRegularsList)
where
import Dojo.Node.EventList
import Dojo.Data.Event
import Dojo.Data.Session
import Dojo.Data.Class
import Dojo.Data.Person
import Dojo.Data.User
import Dojo.Chrome
import Dojo.Paths
import Dojo.Fail
import Dojo.Framework
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Data.Time                      as Time


-------------------------------------------------------------------------------
-- | View a single class, using a vertical form.
--      &cid=NAT   View the class with this id.
cgiClassView :: Session -> [(String, String)] -> CGI CGIResult
cgiClassView ss inputs
 | Just strClassId  <- lookup "cid" inputs
 , Right cid        <- parse strClassId
 = do
        conn        <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        classs      <- liftIO $ getClass conn cid
        events      <- liftIO $ getEventsOfClassId conn cid

        -- Lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner <- liftIO $ getMaybeUser conn uname
        pOwner      <- liftIO $ getPerson conn $ userPersonId uOwner

        -- Lookup regular attendees to this class over the last 90 days.
        zonedTime       <- liftIO $ Time.getZonedTime
        let ltNow       =  Time.zonedTimeToLocalTime zonedTime
        let ltStart
                = ltNow { Time.localDay
                        = Time.addDays (-90) (Time.localDay ltNow) }
        let (edateFirst, _) = splitEventLocalTime ltStart
        regulars        <- liftIO $ getRegularsOfClassId conn cid edateFirst

        liftIO $ disconnect conn

        cgiPageNavi ss "Classes" (classDisplayName classs) (pathsJump ss)
         $ divClassDetails ss cid classs uOwner pOwner events regulars

cgiClassView _ inputs
 = throw $ FailNodeArgs "class view" inputs


-------------------------------------------------------------------------------
-- | Class Details
divClassDetails
        :: Session
        -> ClassId
        -> Class -> User -> Person
        -> [(Event, Int)]                       -- ^ Events in class.
        -> [(Person, Integer, EventDate)]       -- ^ Regular attendees.
        -> Html

divClassDetails
        ss cid classs uOwner pOwner
        eventList regularsList
 = H.div ! A.class_ "class-view"
 $ do
        -- Only bother showing owner user id to admins
        let muOwner
                = if sessionIsAdmin ss
                        then Just uOwner
                        else Nothing
        H.table
         $ do   trClassSummary classs muOwner pOwner

        tableActions
         [ pathNew
         , pathClassEvents   ss cid
         , pathClassRegulars ss cid
         , pathClassDevLink   ss cid]

        H.div ! A.class_ "details"
         $ do
                -- Show events of this class.
                -- TODO: push limit into query.
                divEventList ss    $ take 20 eventList

                -- Show regular attendees
                -- TODO: push limit into query.
                divRegularsList ss $ take 20 regularsList

                -- Try to generate the registration code.
                --  We need to have the type, location day,
                --  start and end times set.
{-
                -- TODO: get website name from global site config.
                let mReg = registrationLinkOfClass
                                "http://dojo.ouroborus.net"
                                (configQrSaltActive $ sessionConfig ss)
                                classs

                (case mReg of
                 Nothing -> return ()
                 Just (sRegLink, sRegId)
                  -> goCode sRegLink sRegId)
-}

 where

  pathNew
    = Path "New Event"
        (sessionCgiName ss)
        [ ("s",         show $ sessionHash ss)
        , ("n",         "eea")
        , ("Location",  maybe "" pretty $ classLocation classs)
        , ("Type",      maybe "" pretty $ classType classs)
        , ("Time",      maybe "" pretty $ classTimeStart classs) ]


-------------------------------------------------------------------------------
-- | Connected summary of class details.
trClassSummary :: Class -> Maybe User -> Person -> Html
trClassSummary classs muOwner pOwner
 = do
        tr $ td $ H.string
           $  maybe "" (\v -> pretty v) (classType classs)
           ++ " class"
           ++ " by "
           ++ maybe "" pretty (personDisplayName pOwner)
           ++ (case muOwner of
                Nothing -> ""
                Just uOwner -> " (" ++ pretty (userName uOwner) ++ ")")
           ++ "."

        tr $ td $ H.string
           $  maybe "[somewhere]" pretty (classLocation classs)
           ++ " on "
           ++ maybe "[someday]"   pretty (classDay classs)
           ++ " at "
           ++ maybe "[sometime]"  pretty (classTimeStart classs)
           ++ maybe "[sometime]"  (\v -> " to " ++ pretty v) (classTimeEnd classs)
           ++ "."


-------------------------------------------------------------------------------
-- | Build list of regular attendees to a class.
divRegularsList
        :: Session
        -> [(Person, Integer, EventDate)]
        -> Html

divRegularsList ss regulars
 = H.div ! A.class_ "list class-regulars"
 $ H.table
 $ do
        col ! A.class_ "Date"
        col ! A.class_ "Name"
        col ! A.class_ "Fees"
        tr $ do th "last attended"
                th "regular attendee"
                th "fees" ! A.style "text-align: center"

        forM_ regulars $ \(person, _nCount, dateLast) -> tr $ do
         td' person (Just dateLast)
         td' person (personDisplayName person)

         let Just pid = personId person
         (H.td ! A.style "text-align: center")
          $ (H.a ! A.href (H.toValue $ pathPersonView ss pid))
          $ H.string $ pretty $ personFeeStatus dateLast person

 where
        td' person val
         = td $ linkView person (H.toMarkup $ maybe "" pretty val)

        linkView person hh
         | Just pid <- personId person
         = (a ! A.href (H.toValue $ pathPersonView ss pid)) hh
         | otherwise = hh

