---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Common where


import           Control.Exception                                           
import           Control.Error                                               
import           Data.Int                                                    
import           Data.IORef                                                  
import           Data.Text.Lazy                                              (Text, pack, unpack)
import qualified Control.Monad.IO.Class                                      

import           Batch_Types                                                 (ArgumentException(..))
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   
import qualified Flowbox.System.UniPath                                    as UniPath


throw' :: String -> c
throw' = throw . ArgumentException . Just . pack


tRunScript :: Script a -> IO a
tRunScript s = do
    e <- runEitherT s
    case e of
        Left  err -> throw' err
        Right a   -> return a


tryReadIORef :: IORef a -> EitherT String IO a
tryReadIORef = scriptIO . readIORef


tryWriteIORef :: Control.Monad.IO.Class.MonadIO m  => IORef a -> a -> EitherT String m ()
tryWriteIORef ref v = scriptIO $ writeIORef ref v


(<?>) :: Monad m => Maybe a -> e -> EitherT e m a
v <?> m = tryRight $ note m v


tryGetID :: Monad m => Maybe Int32 -> String -> EitherT String m Int
tryGetID mtID name = do
    tID <- mtID <?> ("`" ++ name  ++ "` argument is missing")
    return $ i32toi tID


tryGetUniPath :: Monad m => Maybe Text -> String -> EitherT String m UniPath.UniPath
tryGetUniPath mtpath name = do
	tpath <- mtpath <?> ("`" ++ name  ++ "` argument is missing")
	return $ UniPath.fromUnixString $ unpack tpath