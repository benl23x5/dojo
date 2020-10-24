
module Dojo.Framework.QRCode
        (htmlQRCode)
where
import Dojo.Framework
import Data.String
import qualified Codec.Picture                  as CP
import qualified Codec.QRCode                   as QR
import qualified Codec.QRCode.JuicyPixels       as QRP
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Char8          as BC
import qualified Data.ByteString.Base64         as B64
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Text.Blaze.Internal            as I


-- | Html to embed a QR code inline in the page.
htmlQRCode
        :: String  -- ^ Link for when the image is clicked on.
        -> String  -- ^ Base name of the file when the image is downloaded.
                   --   For some name BASE the file will be BASE.png
        -> Html

htmlQRCode sLink sDownloadName
 = do   -- Generate QRCode structure.
        let Just qrimg
                = QR.encodeText
                        (QR.defaultQRCodeOptions QR.L)
                        QR.Utf8WithECI sLink

        -- Produced a scaled PNG image for the code.
        let nBorder = 2
        let nScale  = 20
        let iimg    = QRP.toImage nBorder nScale qrimg
        let bsPng   = CP.encodePng iimg

        -- Encode PNG as Base64 data so we can embed it in the page.
        let bsPng64 = B64.encodeBase64' $ BL.toStrict bsPng
        let ssPng64 = BC.unpack bsPng64
        let ssPage  = "data:image/png;base64, " ++ ssPng64

        -- To get a save-as link with the correct name on an inline image
        -- we seem to need to include the base64 encoded data twice.
        -- Once as the target of the link and once as the image data.
        (H.a ! A.href (fromString ssPage)
             ! I.customAttribute "download"
                (fromString $ sDownloadName ++ ".png"))
         (H.img ! A.class_ "qrcode"
                ! A.style "width:700px;height:700px"
                ! A.id "someimage"
                ! A.src (fromString ssPage))
