
module Dojo.Paths where
import Dojo.Data.Session
import Dojo.Data.Person
import Dojo.Data.Event
import Dojo.Framework
import Config


-- The paths to show at the top of each page.
pathsJump :: Session -> [Path]
pathsJump ss
 = [ pathLogout, pathEventList ss, pathPersonList ss]


-- Session -------------------------------------------------------------------
pathLogin :: Path
pathLogin
 = Path "Login"
        cgiName
        [ ("n", "login") ]


pathLogout :: Path
pathLogout
 = Path "Logout"
        cgiName
        [ ("n", "logout") ]


-- Main -----------------------------------------------------------------------
pathMain :: Session -> Path
pathMain ss
 = Path "Main"
        cgiName
        [ ("s", show $ sessionHash ss)
        , ("n", "main") ]


-- People ---------------------------------------------------------------------
pathPersonList :: Session -> Path
pathPersonList ss
 = Path "People"
        cgiName
        [ ("s", show $ sessionHash ss)
        , ("n", "pList")]


pathPersonView :: Session -> PersonId -> Path
pathPersonView ss (PersonId pid)
 = Path "Done"
        cgiName
        [ ("s", show $ sessionHash ss)
        , ("n", "pView")
        , ("pid", show pid)]


pathPersonAdd :: Session -> Path
pathPersonAdd ss
 = Path "Add Person"
        cgiName
        [ ("s", show $ sessionHash ss)
        , ("n", "pAdd")]


pathPersonEdit :: Session -> PersonId -> Path
pathPersonEdit ss (PersonId pid)
 = Path "Edit Person"
        cgiName
        [ ("s", show $ sessionHash ss)
        , ("n", "pEdit")
        , ("pid", show pid)]


-- Events ---------------------------------------------------------------------
pathEventList :: Session -> Path
pathEventList ss
 = Path "Events"
        cgiName
        [ ("s", show $ sessionHash ss)
        , ("n", "eList")]


pathEventView :: Session -> EventId -> Path
pathEventView ss (EventId eid)
 = Path "Done"
        cgiName
        [ ("s", show $ sessionHash ss)
        , ("n", "eView")
        , ("eid", show eid)]


pathEventAdd :: Session -> Path
pathEventAdd ss
 = Path "Add Event"
        cgiName
        [ ("s", show $ sessionHash ss)
        , ("n", "eEdit")]


pathEventEdit :: Session -> Maybe EventId -> Path
pathEventEdit ss (Just (EventId pid))
 = Path "Edit Event"
        cgiName
        [ ("s",   show $ sessionHash ss)
        , ("n",   "eEdit")
        , ("eid", show pid)]

pathEventEdit ss Nothing
 = Path "Edit Event"
        cgiName
        [ ("s", show $ sessionHash ss)
        , ("n", "eEdit")]


