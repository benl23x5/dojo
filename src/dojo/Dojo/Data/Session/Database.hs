
module Dojo.Data.Session.Database
        ( sessionOfSqlValues 
        , getSessionByHash
        , insertSession)
where
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
sessionOfSqlValues [sid, uid, hash, localStart, localEnd]
        = Session
        { sessionId             = fromSql sid
        , sessionHash           = fromSql hash
        , sessionUserId         = fromSql uid
        , sessionStartDate      = sdate
        , sessionStartTime      = stime
        , sessionEndDate        = edate
        , sessionEndTime        = etime }

        where   (sdate, stime) 
                 = splitSessionLocalTime $ fromSql localStart

                (edate, etime) 
                 = case fromSql localEnd of
                     Nothing     -> (Nothing, Nothing)
                     Just ltime  
                      |  (edate', etime') <- splitSessionLocalTime $ fromSql ltime
                      -> (Just edate', Just etime')


sessionOfSqlValues _ 
        = error "sessionOfValues: no match"


-------------------------------------------------------------------------------
-- | Get the session with the given hash.
getSessionByHash :: IConnection conn => conn -> SessionHash -> IO Session
getSessionByHash conn hash
 = do   [values] <- quickQuery' conn (unlines
                [ "SELECT SessionId,UserId,Hash,StartTime,EndTime"
                , "FROM   Session"
                , "WHERE  Hash=?" ]) 
                [toSql hash]

        return $ sessionOfSqlValues values


-- | Insert a record showing a person attended this event.
insertSession :: IConnection conn  => conn -> Session -> IO Integer
insertSession conn session
 = do   stmt    <- prepare conn $ unlines
                [ "INSERT INTO Session"
                , "(Hash, UserId, StartTime)"
                , "VALUES (?,?,?)" ]

        execute stmt
                [ toSql (sessionHash           session)
                , toSql (sessionUserId         session)
                , toSql (sessionStartLocalTime session) ]

