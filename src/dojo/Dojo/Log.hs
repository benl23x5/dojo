
module Dojo.Log where
import Dojo.Config
import Dojo.Framework.Value
import Dojo.Data.Session
import System.FilePath
import Control.Monad.IO.Class
import Data.List
import Text.Printf
import qualified Data.Time                      as Time


llogc :: MonadIO m => Config -> String -> String -> Value -> m ()
llogc c channel event v
 = liftIO $ logcIO c channel event v

logcIO :: Config -> String -> String -> Value -> IO ()
logcIO c channel event v
 = do   zonedTime <- liftIO $ Time.getZonedTime
        let (date, time)
                = splitSessionLocalTime
                $ Time.zonedTimeToLocalTime zonedTime

        let v'  = O [ ("date",  toValue date)
                    , ("time",  toValue time)
                    , ("event", S event)
                    , ("value", v)]

        appendFile (configPathLog c </> channel)
         $ showValue v' ++ "\n"


llogs :: MonadIO m => Session -> String -> String -> Value -> m ()
llogs ss channel event v
 = liftIO $ logsIO ss channel event v

logsIO :: Session -> String -> String -> Value -> IO ()
logsIO ss channel event v
 = do   zonedTime <- liftIO $ Time.getZonedTime
        let (date, time)
                = splitSessionLocalTime
                $ Time.zonedTimeToLocalTime zonedTime

        let v'  = O [ ("date",  toValue date)
                    , ("time",  toValue time)
                    , ("key",   toValue $ sessionHash ss)
                    , ("event", S event)
                    , ("value", v)]

        appendFile (configPathLog (sessionConfig ss) </> channel)
         $ showValue v' ++ "\n"


showValue :: Value -> String
showValue vv
 = case vv of
        B True  -> "true"
        B False -> "false"
        I i     -> "#i'" ++ show i
        D d     -> "#d'" ++ show d
        S s     -> show s
        O fs    -> "{" ++ intercalate ", " [f ++ " = " ++ showValue v | (f, v) <- fs] ++ "}"
        A vs    -> "[" ++ intercalate ", " [showValue v | v <- vs] ++ "]"
        TD td   -> "#td'" ++ show td

        TT tt
         -> "#tt'"
                ++ printf "%02d:%02d:%02d"
                        (Time.todHour tt)
                        (Time.todMin  tt)
                        ( (truncate $ toRational $ Time.todSec tt) :: Integer)

        TL tl   -> "#tl'" ++ show tl


