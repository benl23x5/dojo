
module Dojo.Data.Session.Database where
import Dojo.Data.Session.Base
import Dojo.Base


-- toSql ----------------------------------------------------------------------
instance Convertible SessionId SqlValue where
 safeConvert (SessionId   sid)          = safeConvert sid

instance Convertible SessionHash SqlValue where
 safeConvert (SessionHash h)            = safeConvert h

instance Convertible SessionTime SqlValue where
 safeConvert (SessionTime tod)          = safeConvert tod

instance Convertible SessionDate SqlValue where
 safeConvert (SessionDate date)         = safeConvert date

instance Convertible SessionLocalTime SqlValue where
 safeConvert (SessionLocalTime etime)   = safeConvert etime


-- fromSql --------------------------------------------------------------------
instance Convertible SqlValue SessionId where
 safeConvert val        = liftM SessionId (safeConvert val)

instance Convertible SqlValue SessionHash where
 safeConvert val        = liftM SessionHash (safeConvert val)

instance Convertible SqlValue SessionTime where
 safeConvert val        = liftM SessionTime (safeConvert val)

instance Convertible SqlValue SessionDate where
 safeConvert val        = liftM SessionDate (safeConvert val)

instance Convertible SqlValue SessionLocalTime where
 safeConvert val        = liftM SessionLocalTime (safeConvert val)


-------------------------------------------------------------------------------
-- | Build an event from a list of Sql values.
sessionOfSqlValues :: [SqlValue] -> Session
sessionOfSqlValues
 [ sid, hash
 , uid, roleNative, roleActive
 , localStart, localEnd]
 = Session
        { sessionId             = fromSql sid
        , sessionHash           = fromSql hash
        , sessionUserId         = fromSql uid
        , sessionRoleNative     = fromSql roleNative
        , sessionRoleActive     = fromSql roleActive
        , sessionStartDate      = sdate
        , sessionStartTime      = stime
        , sessionEndDate        = edate
        , sessionEndTime        = etime }

 where  (sdate, stime)
         = splitSessionLocalTime $ fromSql localStart

        (edate, etime)
         = case fromSql localEnd of
            Nothing -> (Nothing, Nothing)
            Just ltime
             |  (edate', etime') <- splitSessionLocalTime $ fromSql ltime
             -> (Just edate', Just etime')

sessionOfSqlValues _
        = error "sessionOfValues: no match"


-------------------------------------------------------------------------------
-- | Get the session with the given hash.
getSessionByHash
        :: IConnection conn
        => conn -> SessionHash
        -> IO (Maybe Session)

getSessionByHash conn hash
 = do   vss     <- quickQuery' conn (unlines
                [ "SELECT SessionId, Hash,"
                , "       UserId, RoleNative, RoleActive,"
                , "       StartTime, EndTime"
                , "FROM   v1_Session"
                , "WHERE  Hash=? AND EndTime IS NULL"])
                [toSql hash]

        case vss of
         [vs]   -> return $ Just $ sessionOfSqlValues vs
         _      -> return Nothing


-- | Insert a record showing a person attended this event.
insertSession :: IConnection conn  => conn -> Session -> IO Integer
insertSession conn session
 = do   stmt    <- prepare conn $ unlines
                [ "INSERT INTO v1_Session"
                , "(Hash, UserId, RoleNative, RoleActive, StartTime)"
                , "VALUES (?,?,?,?,?)" ]

        execute stmt
                [ toSql (sessionHash            session)
                , toSql (sessionUserId          session)
                , toSql (sessionRoleNative      session)
                , toSql (sessionRoleActive      session)
                , toSql (sessionStartLocalTime  session) ]


-- | Update the session with the given end time.
--   The end date and time fields are overwritten with the given ones.
endSession
        :: IConnection conn
        => conn -> SessionDate -> SessionTime -> Session -> IO Session
endSession conn sdate stime session
 = do
        -- update the session end date/time fields.
        let session'
                = session
                { sessionEndDate        = Just sdate
                , sessionEndTime        = Just stime }

        -- will always succeed as we set the end date/time above.
        let Just eltime = sessionEndLocalTime session'

        stmt    <- prepare conn $ unlines
                [ "UPDATE v1_Session"
                , "SET   EndTime=?"
                , "WHERE SessionId=?" ]

        execute stmt
                [ toSql eltime
                , toSql (sessionId session) ]

        return session'
