
module Dojo.Node.ClassDevReg where
import Dojo.Data.Person
import Dojo.Data.User
import Dojo.Data.Class
import Dojo.Node.ClassView
import Dojo.Node.PersonDevStatus
import Dojo.Chrome
import Dojo.Fail
import Dojo.Config
import Dojo.Framework
import Dojo.Paths
import Data.String
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


-------------------------------------------------------------------------------
-- | Class device registration.
cgiClassDevReg :: Config -> String -> CGI CGIResult
cgiClassDevReg cc sRegId
 | strClassId  <- sRegId                        -- TODO: proper reg. code.
 , Right cid   <- parse strClassId
 = do
        conn    <- liftIO $ connectSqlite3 $ configDatabasePath cc

        -- Class details based on visited URL.
        classs  <- liftIO $ getClass conn cid

        -- Person information based on cookie set in the browser.
        mCookiePerson <- cgiGetPersonOfStudentCookie cc

        -- Lookup details of the class owner.
        let Just uname = classOwnerUserName classs
        Just uOwner     <- liftIO $ getMaybeUser conn uname
        pOwner          <- liftIO $ getPerson conn $ userPersonId uOwner

        let bRegistered = False

        cgiClassStatus cc classs pOwner mCookiePerson bRegistered


cgiClassDevReg _ _
 = throw $ FailNodeArgs "class device registration" []


-------------------------------------------------------------------------------
cgiClassStatus
        :: Config
        -> Class -> Person      -- ^ Class details, and class owner.
        -> Maybe Person         -- ^ Person record from device cookie, if any.
        -> Bool                 -- ^ Person is recorded as attending class.
        -> CGI CGIResult

cgiClassStatus cc classs pClassOwner mpCookie bAttending
 = cgiPagePlain (classDisplayName classs)
 $ H.div ! A.class_ "class-dev-reg"
 $ do
        H.img
         ! (A.src $ fromString $ configLogoUrl cc)
         ! (A.style "width:450px;height:450px")
         ! (A.id    "login-logo")

        H.br
        H.div ! A.class_ "code-description"
         $ H.table $ do
                trClassSummary classs Nothing pClassOwner

                case mpCookie of
                 Nothing -> do
                   tr $ td $ H.h2 $ (H.p ! A.style "color:Brown;")
                      $ do H.string "Device not Registered "
                           (H.i ! A.class_ "material-icons md-48")
                            $ "sentiment_dissatisfied"

                   tr $ td $ "This device is not registered"
                   tr $ td $ "for the class attendance system."
                   tr $ td $ "Please contact the class instructor."

                 Just pCookie -> do
                   let sName = fromMaybe "[person]" $ personAliasName pCookie
                   tr $ td ! A.style "height:1em;" $ ""
                   tr $ td $ H.h3 $ H.string sName
                   tr $ td ! A.style "height:1ex;" $ H.string ""

                   if bAttending
                    then do
                     tr $ td $ H.h2 $ (H.div ! A.style "color:Green;")
                        $ do H.string "Present "
                             (H.i ! A.class_ "material-icons md-48")
                               $ "sentiment_satisfied_alt"

                     tr $ td ! A.style "height:1ex;" $ H.string ""

                    else H.table $ do
                     tr $ td $ H.h2 $ (H.div ! A.style "color:Brown;")
                        $ do H.string "Not Attending "
                             (H.i ! A.class_ "material-icons md-48")
                               $ "sentiment_dissatisfied"

                     let Just cid  = classId classs
                     let pathClass = pathClassDevReg cc cid
                     tr $ td
                        $ (H.form    ! A.action (fromString $ flatten $ pathClass))
                        $ do H.input ! A.type_ "hidden"
                                     ! A.name  (fromString "r")
                                     ! A.value (fromString $ pretty cid)

                             H.input ! A.type_ "hidden"
                                     ! A.name (fromString "attend")

                             H.input ! A.type_ "submit"
                                     ! A.value "Attend Class"
