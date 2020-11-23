
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
                tr $ td $ H.h3 $ H.string sName
                tr $ td $ H.string "Link to student device registration."
                tr $ td $ H.string ""

                -- Base name of the file to use if the QR code .png image
                -- is downloaded.
                let sCodeFileName = personQRCodeDownloadName person

                -- Generate the QR code inline in the page.
                tr $ td $ htmlQRCode sLink sCodeFileName

                tr $ td $ H.string "The student should scan this code"
                tr $ td $ H.string "and be directed to the page to"
                tr $ td $ H.string "register their own device."
                tr $ td $ H.string $ "code id: " ++ sCode

                tr $ td ! A.style "height:1ex;" $ H.string ""
                tr $ td $ H.string "The direct link is:"
                tr $ td $ (H.a ! A.href (H.toValue sLink)) (H.string sLink)

                tr $ td ! A.style "height:1ex;" $ H.string ""
                tr $ td $ H.string "Registration PDF download:"
                tr $ td $ (H.a ! A.href (H.toValue $ "generated/" ++ sRegDownload))
                                (H.string sRegDownload)

cgiPersonDevLink _ inputs
 = throw $ FailNodeArgs "person device link" inputs


---------------------------------------------------------------------------------------------------
-- | Build the per-person device registration PDF and return the relative URL to it.
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
--  TODO: disable browsing of generated file dir
--  TODO: salt the name of the build directory so each build has a new name.
--  TODO: cleanup build directory if the latex command fails.
--  TODO: cleanup generated files once they have been around for a day,
--        or there are too many.
--
buildPersonRegPDF
 :: Config
 -> PersonId            -- ^ Person id used to create build path.
 -> Person
 -> String              -- ^ Device registration code.
 -> BS.ByteString       -- ^ PNG file contents for QR image.
 -> IO FilePath

buildPersonRegPDF config (PersonId ppid) person sCode bsPngCode
 = do
        -- Create per-person build directory.
        let pathBuildForPerson
                = configPathBuild config
                ++ "/reg-pid-" ++ show ppid ++ "-" ++ sCode

        S.createDirectoryIfMissing True
         $ pathBuildForPerson

        -- Where the latex template is copied to.
        let pathBuildForPersonLatex
                = pathBuildForPerson ++ "/latex"

        -- Where the per-person reg file is built.
        let pathBuiltPDF
                = pathBuildForPersonLatex ++ "/main.pdf"

        -- Where the generated file is stored for download.
        S.createDirectoryIfMissing True
         $ configPathHtml config ++ "/generated"

        let Just sDisplayName
                = personDisplayName person

        let sFlatName       = filter (not . Char.isSpace) sDisplayName
        let sFileName       = "student" ++ "-" ++ sCode ++ "-" ++ sFlatName ++ ".pdf"
        let pathHostedPDF   = configPathHtml config ++ "/generated/" ++ sFileName

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
         $ do   S.readProcess
                        "/usr/bin/pdflatex"
                        [ "main.tex"]
                        ""

        -- Copy out the result into the host directory.
        S.rawSystem "cp" [ pathBuiltPDF, pathHostedPDF ]

        -- Remove leftover build files.
        S.removeDirectoryRecursive pathBuildForPerson

        return sFileName

