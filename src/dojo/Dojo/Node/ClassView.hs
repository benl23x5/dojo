
module Dojo.Node.ClassView (cgiClassView) where
import Dojo.Data.Session
import Dojo.Data.Class
import Dojo.Framework
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


-------------------------------------------------------------------------------
-- | View a single class, using a vertical form.
--      &cid=NAT   View the class with this id.
cgiClassView
        :: Session -> [(String, String)]
        -> CGI CGIResult

cgiClassView ss inputs
 | Just strClassId  <- lookup "cid" inputs
 , Right cid        <- parse strClassId
 = do   conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        classs  <- liftIO $ getClass conn cid
        liftIO $ disconnect conn

        cgiClassView_page ss classs

 | otherwise
 = throw $ FailNodeArgs "class view" inputs


cgiClassView_page ss classs
 = outputFPS $ renderHtml
 $ H.docTypeHtml
 $ do   pageHeader $ classDisplayName classs
        pageBody
         $ do   tablePaths $ pathsJump ss

                tablePaths
                 [ Path "New Event of Class"
                        (sessionCgiName ss)
                        [ ("s",         show $ sessionHash ss)
                        , ("n",         "ee")
                        , ("Location",  maybe "" pretty $ classLocation classs)
                        , ("Type",      maybe "" pretty $ classType classs)
                        , ("Time",      maybe "" pretty $ classTimeStart classs) ]]

                divClassDetails ss classs


-------------------------------------------------------------------------------
-- | Class Details
divClassDetails :: Session -> Class -> Html
divClassDetails ss classs
 = H.div ! A.class_ "details" ! A.id "class-details-view"
 $ do
        H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "location"; th "day"
                tr $ do td' $ classLocation classs
                        td' $ classDay classs

        H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "start time"; th "end time"
                tr $ do td' $ classTimeStart classs
                        td' $ classTimeEnd classs

        H.table
         $ do   tr $ do th "type"
                tr $ do td' $ classType classs

        -- First / final date tracking only matters when searching
        -- for events, so only relevant to admins.
        when (sessionIsAdmin ss)
         $ H.table
         $ do   col ! A.class_ "Col2A"; col ! A.class_ "Col2B"
                tr $ do th "first date"; th "final date"
                tr $ do td' $ classDateFirst classs
                        td' $ classDateFinal classs

        -- Generate inline QR image.
        let ssContent :: String
                = "http://dojo.ouroborus.net?r=abcdefgh"

        let Just qrimg
                = QR.encodeText
                        (QR.defaultQRCodeOptions QR.L)
                        QR.Utf8WithECI ssContent

        let iimg    = QRP.toImage 2 20 qrimg
        let bsPng   = CP.encodePng iimg
        let bsPng64 = B64.encodeBase64' $ BL.toStrict bsPng
        let ssPng64 = BC.unpack bsPng64
        let ssPage  = "data:image/png;base64, " ++ ssPng64
        H.table
         $ do   tr $ do th "registration QR code"
                tr $ td $ do
                        (H.img ! A.class_ "qrcode" ! A.src (fromString ssPage))

 where  td' val = td $ H.toMarkup $ maybe "" pretty $ val

