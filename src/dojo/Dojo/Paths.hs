
module Dojo.Paths where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.Class
import Dojo.Framework
import Dojo.Config

-- The paths to show at the top of each page.
pathsJump :: Session -> [Path]
pathsJump ss
 =      [ pathPersonList ss
        , pathClassList  ss
        , pathEventList  ss
        , pathLogout     ss]


-- Debug -------------------------------------------------------------------
pathDebug :: Config -> String -> Path
pathDebug config junk
 = Path "Debug"
        (configCgiName config)
        [ ("d", junk) ]


-- Session -------------------------------------------------------------------
pathLogin :: Config -> Path
pathLogin config
 = Path "Login"
        (configCgiName config)
        [ ("n", "login") ]


pathLogout :: Session -> Path
pathLogout ss
 = Path "Logout"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "logout") ]


-- Main -----------------------------------------------------------------------
pathRegister :: Config -> String -> Path
pathRegister cc sRegId
 = Path "Register"
        (configCgiName cc)
        [ ("r", sRegId) ]


-- Main -----------------------------------------------------------------------
pathMain :: Session -> Path
pathMain ss
 = Path "Main"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "main") ]


-- People ---------------------------------------------------------------------
pathPersonList :: Session -> Path
pathPersonList ss
 = Path "People"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "pl")]


pathPersonAdd :: Session -> Path
pathPersonAdd ss
 = Path "Add Person"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "pe")]


pathPersonDel :: Session -> PersonId -> Path
pathPersonDel ss (PersonId pid)
 = Path "Delete Person"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "pd")
        , ("pid", show pid) ]


pathPersonEdit :: Session -> Maybe PersonId -> Path
pathPersonEdit ss (Just (PersonId pid))
 = Path "Edit Person"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "pe")
        , ("pid", show pid)]

pathPersonEdit ss Nothing
 = Path "Edit Person"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "pe") ]


pathPersonView :: Session -> PersonId -> Path
pathPersonView ss (PersonId pid)
 = Path "View Person"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "pv")
        , ("pid", show pid)]


pathPersonDiscard :: Session -> PersonId -> Path
pathPersonDiscard ss (PersonId pid)
 = Path "Discard Change"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "pv")
        , ("pid", show pid)]


-- | Page to show QR code that redirects to the device registration page.
pathPersonDevLink :: Session -> PersonId -> Path
pathPersonDevLink ss (PersonId pid)
 = Path "Device Reg."
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "pdl")
        , ("pid", show pid) ]


-- | The actual device registration page that will set the reg. cookie.
pathPersonDevStatus :: Config -> String-> Path
pathPersonDevStatus cc sCode
 = Path "Device Status."
        (configCgiName cc)
        [ ("pds", sCode) ]


-- Events ---------------------------------------------------------------------
pathEventList :: Session -> Path
pathEventList ss
 = Path "Events"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "el")]


pathEventView :: Session -> EventId -> Path
pathEventView ss (EventId eid)
 = Path "View Event"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "ev")
        , ("eid", show eid)]


pathEventAdd :: Session -> Path
pathEventAdd ss
 = Path "Add Event"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "eed")]


pathEventDel :: Session -> EventId -> Path
pathEventDel ss (EventId eid)
 = Path "Delete Event"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "ed")
        , ("eid", show eid) ]


pathEventEditDetails :: Session -> Maybe EventId -> Path
pathEventEditDetails ss (Just (EventId eid))
 = Path "Edit Event"
        (sessionCgiName ss)
        [ ("s",   show $ sessionHash ss)
        , ("n",   "eed")
        , ("eid", show eid)]

pathEventEditDetails ss Nothing
 = Path "Edit Event"
        (sessionCgiName ss)
        [ ("s",   show $ sessionHash ss)
        , ("n",   "eed")]


pathEventEditAttend :: Session -> Maybe EventId -> Path
pathEventEditAttend ss (Just (EventId eid))
 = Path "Edit Attendance"
        (sessionCgiName ss)
        [ ("s",   show $ sessionHash ss)
        , ("n",   "eea")
        , ("eid", show eid)]

pathEventEditAttend ss Nothing
 = Path "Edit Attendance"
        (sessionCgiName ss)
        [ ("s",   show $ sessionHash ss)
        , ("n",   "eea")]


pathEventEdit :: Session -> Maybe EventId -> Path
pathEventEdit ss (Just (EventId eid))
 = Path "Edit Event"
        (sessionCgiName ss)
        [ ("s",   show $ sessionHash ss)
        , ("n",   "ee")
        , ("eid", show eid)]

pathEventEdit ss Nothing
 = Path "Edit Event"
        (sessionCgiName ss)
        [ ("s",   show $ sessionHash ss)
        , ("n",   "ee")]



-- Classes ---------------------------------------------------------------------
pathClassList :: Session -> Path
pathClassList ss
 = Path "Classes"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "cl")]


pathClassView :: Session -> ClassId -> Path
pathClassView ss (ClassId cid)
 = Path "View Class"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "cv")
        , ("cid", show cid)]


pathClassEvents :: Session -> ClassId -> Path
pathClassEvents ss (ClassId cid)
 = Path "Events"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "ce")
        , ("cid", show cid)]


pathClassRegulars :: Session -> ClassId -> Path
pathClassRegulars ss (ClassId cid)
 = Path "Regulars"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "cr")
        , ("cid", show cid)]


-- | Page to show class QR registration code.
pathClassDevLink :: Session -> ClassId -> Path
pathClassDevLink ss (ClassId cid)
 = Path "Reg. Code"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "cdl")
        , ("cid", show cid) ]


-- | Page student should access to register for the class.
pathClassDevReg :: Config -> Class -> Path
pathClassDevReg cc classs
 = let  sUrl  = configSiteUrl cc
        sSalt = configQrSaltActive cc
        Just (_, sRegId) = registrationLinkOfClass sUrl sSalt classs
    in Path "Reg. Code"
        (configCgiName cc)
        [ ("r", sRegId) ]

