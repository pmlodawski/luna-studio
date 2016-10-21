module ZMQ.RPC.RPC where

import           Control.Exception     (SomeException, try)
import           Control.Monad         (join)

import           Flowbox.Control.Error hiding (err)
import           Flowbox.Prelude



type RPC a = ExceptT Error IO a


type Error = String



run :: MonadIO m => RPC r -> m (Either Error r)
run action = do
    result <- liftIO $ (try :: IO a -> IO (Either SomeException a)) $ runExceptT action
    return $ join $ fmapL (\exception -> "Unhandled exception: " ++ show exception) result
