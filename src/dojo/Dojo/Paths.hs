
module Dojo.Paths where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Framework
import Dojo.Config

-- The paths to show at the top of each page.
pathsJump :: Session -> [Path]
pathsJump ss
 =      [ pathPersonList ss
        , pathClassList  ss
        , pathEventList  ss
        , pathLogout     ss]


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
        , ("n", "ee")]


pathEventDel :: Session -> EventId -> Path
pathEventDel ss (EventId eid)
 = Path "Del Event"
        (sessionCgiName ss)
        [ ("s", show $ sessionHash ss)
        , ("n", "ed")
        , ("eid", show eid) ]


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
