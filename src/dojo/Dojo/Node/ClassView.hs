
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
import Dojo.Config
import Dojo.Chrome
import Dojo.Paths
import Dojo.Fail
import Data.String
import Dojo.Framework
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

import qualified Codec.Picture                  as CP
import qualified Codec.QRCode                   as QR
import qualified Codec.QRCode.JuicyPixels       as QRP
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Char8          as BC
import qualified Data.ByteString.Base64         as B64
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

        cgiPageNavi "Classes" (classDisplayName classs) (pathsJump ss)
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
        H.table
         $ do   trClassSummary classs uOwner pOwner

        tableActions
         [ pathNew
         , pathClassEvents ss cid
         , pathClassRegulars ss cid ]

        -- First / final date tracking only matters when searching
        -- for events, so only relevant to admins.
--      when (sessionIsAdmin ss)
--       $ H.table
--       $ do   col ! A.class_ "Col2bA"
--              col ! A.class_ "Col2bB"
--              tr $ do th "first event date"; th "final event date"
--              tr $ do td' $ classDateFirst classs
--                      td' $ classDateFinal classs

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
        , ("n",         "eed")
        , ("Location",  maybe "" pretty $ classLocation classs)
        , ("Type",      maybe "" pretty $ classType classs)
        , ("Time",      maybe "" pretty $ classTimeStart classs) ]

  goCode sRegLink sRegId
   = do -- Generate inline QR image.
        let Just qrimg
                = QR.encodeText
                        (QR.defaultQRCodeOptions QR.L)
                        QR.Utf8WithECI sRegLink

        let iimg    = QRP.toImage 2 20 qrimg
        let bsPng   = CP.encodePng iimg
        let bsPng64 = B64.encodeBase64' $ BL.toStrict bsPng
        let ssPng64 = BC.unpack bsPng64
        let ssPage  = "data:image/png;base64, " ++ ssPng64

        H.div ! A.class_ "registration-code"
         $ H.table
         $ do   tr $ do th "registration QR code"
                tr $ td $ (H.a ! A.href (H.toValue sRegLink))
                           (H.img ! A.class_ "qrcode" ! A.src (fromString ssPage))
                tr $ td $ (H.a ! A.href (H.toValue sRegLink)) "Registration Page"

        H.div ! A.class_ "registration-id"
         $ H.table
         $ do   tr $ th "registration QR identifier"
                tr $ td $ (H.div ! A.class_ "qrident") $ H.string sRegId


-------------------------------------------------------------------------------
-- | Connected summary of class details.
trClassSummary :: Class -> User -> Person -> Html
trClassSummary classs uOwner pOwner
 = do
        tr $ td $ H.string
           $  maybe "" (\v -> pretty v) (classType classs)
           ++ " class"
           ++ " by "
           ++ maybe "" pretty (personDisplayName pOwner)
           ++ " (" ++ pretty (userName uOwner) ++ ")"
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

