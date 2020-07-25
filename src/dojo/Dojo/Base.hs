
-- | Setup the top-level namespace for web development.
--   All external dependencies are imported via this module.
module Dojo.Base
        ( module Network.CGI
        , module Database.HDBC
        , module Database.HDBC.Sqlite3
        , module Text.Blaze
        , module Text.Blaze.Html5
        , module Text.Blaze.Html.Renderer.Utf8
        , module Data.Char
        , module Data.List
        , module Data.Maybe
        , module Data.Convertible
        , module Control.Monad
        , module Control.Exception
        , module Prelude
        , (!?)
        , takeHead)
where
import Network.CGI
        (CGI, CGIResult, liftIO, outputFPS, redirect)

import Database.HDBC
        ( SqlValue, toSql, fromSql
        , IConnection
        , withTransaction, commit, disconnect, prepare, execute, quickQuery')

import Database.HDBC.Sqlite3
        (connectSqlite3)

import Text.Blaze
        hiding ((!?))

import Text.Blaze.Html5
        hiding (map, span, (!?), main)

import Text.Blaze.Html.Renderer.Utf8
        (renderHtml)

import Data.Char
import Data.List
        hiding (head, span)

import Data.Maybe

import Data.Convertible

import Control.Monad

import Control.Exception

import Prelude
        hiding (head, tail, last, undefined, span, div)

(!?) x (enable, attr, v)
        | enable        = x ! attr v
        | otherwise     = x


takeHead :: [a] -> Maybe a
takeHead [] = Nothing
takeHead (x : _) = Just x

