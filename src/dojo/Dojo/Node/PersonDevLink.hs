
module Dojo.Node.PersonDevLink (cgiPersonDevLink) where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Chrome
import Dojo.Config
import Dojo.Paths
import Dojo.Fail
import Dojo.Framework
import Dojo.Framework.QRCode
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A

import qualified System.Directory               as S
import qualified System.Process                 as S
import qualified System.IO                      as S
import qualified Data.Char                      as Char
import qualified Data.ByteString                as BS


---------------------------------------------------------------------------------------------------
-- | Show the device registration QR code for a person.
--      &pid=NAT   Id of the person to show code for.
cgiPersonDevLink
 :: Session -> [(String, String)]
 -> CGI CGIResult

cgiPersonDevLink ss inputs
 | Just strPersonId <- lookup "pid" inputs
 , Right pid    <- parse strPersonId
 = do
        let config = sessionConfig ss
        conn    <- liftIO $ connectSqlite3 $ sessionDatabasePath ss
        person  <- liftIO $ getPerson conn pid
        sCode   <- liftIO $ acquirePersonDeviceRegCode conn
                                (configQrSaltActive config) pid
        liftIO $ commit conn
        liftIO $ disconnect conn

        let cc          = sessionConfig ss
        let pRegStatus  = pathPersonDevStatus (sessionConfig ss) sCode
        let sLink       = configSiteUrl cc ++ "/" ++ flatten pRegStatus

        let sCodePng = makeQRCode sLink
        sRegDownload
         <- liftIO $ buildPersonRegPDF config pid person sCode sCodePng

        let sName = fromMaybe "[person]" $ personAliasName person
        cgiPageNavi ss "People" sName (pathsJump ss)
         $ H.div ! A.class_ "code-description"
         $ do H.br; H.table $ do
                H.tr $ H.td $ H.h3 $ H.string sName
                H.tr $ H.td $ H.string "Link to student device registration."
                H.tr $ H.td $ H.string ""

                -- Base name of the file to use if the QR code .png image
                -- is downloaded.
                let sCodeFileName = personQRCodeDownloadName person

                -- Generate the QR code inline in the page.
                H.tr $ H.td $ htmlQRCode sLink sCodeFileName

                H.tr $ H.td $ H.string "The student should scan this code"
                H.tr $ H.td $ H.string "and be directed to the page to"
                H.tr $ H.td $ H.string "register their own device."
                H.tr $ H.td $ H.string $ "code id: " ++ sCode

                H.tr $ H.td ! A.style "height:1ex;" $ H.string ""
                H.tr $ H.td $ H.string "The direct link is:"
                H.tr $ H.td $ (H.a ! A.href (H.toValue sLink)) (H.string sLink)

                H.tr $ H.td ! A.style "height:1ex;" $ H.string ""
                H.tr $ H.td $ H.string "Registration PDF download:"
                H.tr $ H.td $ (H.a ! A.href (H.toValue $ "g/" ++ sRegDownload))
                                (H.string sRegDownload)

cgiPersonDevLink _ inputs
 = throw $ FailNodeArgs "person device link" inputs


---------------------------------------------------------------------------------------------------
-- | Build the per-person device registration PDF and return the
--   relative URL to it.
--
--   Latex template for the pdf is specified in configPathLatexPerson
--    The template has files:
--          figure/qr-student-ident.tex
--      and figure/qr-student-image.png
--    which we overwrite with the real student name and the device
--    registration QR code for them, before calling pdflatex
--    on the main.tex file.
--
--   The resulting pdf file main.pdf is copied out to URL
--   /generated/register-CODE_ID-PERSON_NAME.pdf
--
--  TODO: cleanup generated files once they have been around for a day,
--        or there are too many.
--
buildPersonRegPDF
 :: Config
 -> PersonId        -- ^ Person id used to create build path.
 -> Person          -- ^ Person to build the QR registration sheet for.
 -> String          -- ^ Device registration code.
 -> BS.ByteString   -- ^ PNG file contents for QR image.
 -> IO FilePath     -- ^ Public URL to the hosted QR registration sheet.

buildPersonRegPDF config (PersonId ppid) person sCode bsPngCode
 = do
        -- Create per-person random build salt, which we use to ensure that
        -- concurrent builds of the same document will not conflict.
        sBuildSalt
         <- newRandomDeviceCode (configQrSaltActive config)

        -- Create the person person build directory for the latex document.
        --  We copy in the template, update it with the current person name,
        --  then call latex in this directory.
        let pathBuildForPerson
                = configPathBuild config
                ++ "/reg-pid-" ++ show ppid ++ "-" ++ sBuildSalt ++ "-" ++ sCode

        S.createDirectoryIfMissing True
         $ pathBuildForPerson

        -- Where the latex template itself is copied to.
        let pathBuildForPersonLatex
                = pathBuildForPerson ++ "/latex"

        -- Where the generated PDF file is written.
        let pathBuiltPDF
                = pathBuildForPersonLatex ++ "/main.pdf"

        -- Where the generated PDF file is stored for download,
        -- in a publically accessible web path.
        S.createDirectoryIfMissing True
         $ configPathHtml config ++ "/g"

        -- Full path to the generated PDF.
        let Just sDisplayName
                = personDisplayName person

        let sFlatName     = filter (not . Char.isSpace) sDisplayName
        let sFileName     = "student" ++ "-" ++ sCode ++ "-" ++ sFlatName ++ ".pdf"
        let pathHostedPDF = configPathHtml config ++ "/g/" ++ sFileName

        -- Copy latex document template into the build directory.
        S.rawSystem "cp"
                [ "-r"
                , configPathLatexPerson config
                , pathBuildForPersonLatex]

        -- Write student name into template.
        S.writeFile
         (pathBuildForPersonLatex ++ "/figure/qr-student-ident.tex")
         ("{\\Huge \\textbf " ++ sDisplayName ++ "}")

        -- Write student QR registration code into template.
        BS.writeFile
         (pathBuildForPersonLatex ++ "/figure/qr-student-image.png")
         bsPngCode

        -- Call pdflatex latex to build the document.
        S.withCurrentDirectory
                pathBuildForPersonLatex
         $ S.readProcess
                "/usr/bin/pdflatex"
                [ "main.tex"]
                ""

        -- Copy out the result into the host directory.
        S.rawSystem "cp" [ pathBuiltPDF, pathHostedPDF ]

        -- Remove leftover build files.
        --  This runs indepdendently of whether the latex command completed
        --  successfully or not.
        S.removeDirectoryRecursive pathBuildForPerson

        return sFileName

