
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


-- | Html to embed a QR code inline in the page.
htmlQRCode :: String -> Html
htmlQRCode sLink
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

        (H.a ! A.href (H.toValue sLink))
                (H.img ! A.class_ "qrcode" ! A.src (fromString ssPage))
