
module Dojo.Data.Session
        ( Session               (..)
        , SessionId             (..)
        , SessionTime           (..)
        , SessionHash           (..)

        -- * Constructors
        , makeSession

        -- * Projections
        , sessionStartLocalTime

        -- * Conversions
        , makeSessionLocalTime
        , splitSessionLocalTime

        -- * Database
        , sessionOfSqlValues
        , getSessionByHash
        , insertSession)
where
import Dojo.Data.Session.Base
import Dojo.Data.Session.Database


instance Show SessionHash where
 show (SessionHash hash) = hash
