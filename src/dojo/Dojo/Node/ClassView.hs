
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
cgiClassView
        :: Session -> [(String, String)]
        -> CGI CGIResult

cgiClassView ss inputs
 | Just strClassId  <- lookup "cid" inputs
 , Right cid        <- parse strClassId
 = do
        conn        <- liftIO $ connectSqlite3 $ sessionDatabasePath ss

        -- lookup class details and events of this class.
        classs      <- liftIO $ getClass conn cid
        events      <- liftIO $ getEventsOfClassId conn cid

        -- lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner <- liftIO $ getMaybeUser conn uname
        pOwner      <- liftIO $ getPerson conn $ userPersonId uOwner

        -- lookup regular attendees to this class over the last 90 days.
        zonedTime       <- liftIO $ Time.getZonedTime
        let ltNow       =  Time.zonedTimeToLocalTime zonedTime
        let ltStart
                = ltNow { Time.localDay
                        = Time.addDays (-90) (Time.localDay ltNow) }
        let (edateFirst, _) = splitEventLocalTime ltStart
        regulars        <- liftIO $ getRegularsOfClassId conn cid edateFirst

        liftIO $ disconnect conn

        cgiClassView_page ss cid classs events uOwner pOwner regulars

 | otherwise
 = throw $ FailNodeArgs "class view" inputs


cgiClassView_page ss cid classs events uOwner pOwner regulars
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader $ classDisplayName classs
        pageBody
         $ do   tablePaths $ pathsJump ss
                divClassDetails ss cid classs uOwner pOwner events regulars


-- TODO: export this separately to the register node.
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
 = H.div ! A.class_ "details" ! A.id "class-details-view"
 $ do
        H.table
         $ do   tr $ th "class"
                trClassSummary classs uOwner pOwner

                tr $ td $ (H.a ! A.href (H.toValue pathNew))
                        $ H.toMarkup $ pathName pathNew

        -- First / final date tracking only matters when searching
        -- for events, so only relevant to admins.
--      when (sessionIsAdmin ss)
--       $ H.table
--       $ do   col ! A.class_ "Col2bA"
--              col ! A.class_ "Col2bB"
--              tr $ do th "first event date"; th "final event date"
--              tr $ do td' $ classDateFirst classs
--                      td' $ classDateFinal classs


        -- Show events of this class.
        -- TODO: push limit into query.
        divEventList ss    $ take 20 eventList
        tablePaths [pathClassEvents ss cid ]

        -- Show regular attendees
        -- TODO: push limit into query.
        divRegularsList ss $ take 20 regularsList
        tablePaths [pathClassRegulars ss cid]

        -- Try to generate the registration code.
        --  We need to have the type, location day,
        --  start and end times set.

        -- TODO: get website name from global site config.
        let mReg = registrationLinkOfClass
                        "http://dojo.ouroborus.net"
                        (configQrSaltActive $ sessionConfig ss)
                        classs

        (case mReg of
         Nothing -> return ()
         Just (sRegLink, sRegId)
          -> goCode sRegLink sRegId)

 where
-- td' val = td $ H.toMarkup $ maybe "" pretty $ val

  pathNew
    = Path "Create New Event of Class"
        (sessionCgiName ss)
        [ ("s",         show $ sessionHash ss)
        , ("n",         "ee")
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

        H.table
         $ do   tr $ do th "registration QR code"
                tr $ td $ (H.a ! A.href (H.toValue sRegLink))
                           (H.img ! A.class_ "qrcode" ! A.src (fromString ssPage))

        H.table
         $ do   tr $ th "registration QR identifier"
                tr $ td $ (H.div ! A.class_ "qrident") $ H.string sRegId


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
        tr $ do th "last attended"; th "name"

        forM_ regulars $ \(person, _nCount, dateLast) -> tr $ do
         td' person (Just dateLast)
         td' person (personDisplayName person)


 where  td' person val
         | Just pid <- personId person
         = td $ (a ! A.href (H.toValue $ pathPersonView ss pid))
                (H.toMarkup $ maybe "" pretty $ val)

         | otherwise
         = td (H.toMarkup $ maybe "" pretty $ val)