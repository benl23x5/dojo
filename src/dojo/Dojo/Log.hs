
module Dojo.Log where
import Dojo.Config
import Dojo.Framework.Value
import System.FilePath
import Control.Monad.IO.Class
import Data.List

llog :: MonadIO m => Config -> FilePath -> Value -> m ()
llog c sub v
 = liftIO $ logIO c sub v


logIO :: Config -> FilePath -> Value -> IO ()
logIO c sub v
 = appendFile (configPathLog c </> sub) (showValue v)


showValue :: Value -> String
showValue vv
 = case vv of
        B True  -> "true"
        B False -> "false"
        I i     -> "i'" ++ show i
        D d     -> "d'" ++ show d
        S s     -> s
        O fs    -> "{" ++ intercalate ", " [show f ++ " = " ++ showValue v | (f, v) <- fs] ++ "}"
        A vs    -> "[" ++ intercalate ", " [show v | v <- vs] ++ "}"



