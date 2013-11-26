---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Control.Error (
    module Control.Error,
    runScript,
    (<?>),
    (<??>),
    ifnot,
    tryReadIORef,
    tryWriteIORef,
    tryGetID,
    tryGetString,
    tryGetUniPath,
) where

import           Control.Error                   hiding (runScript)
import           Data.Int                          
import           Data.IORef                        
import           Data.Text.Lazy                    (Text, unpack)
import qualified Control.Monad.IO.Class            

import           Flowbox.Prelude                   
import qualified Flowbox.System.UniPath          as UniPath
import           Flowbox.Tools.Conversion.Common   


runScript :: Script a -> IO a
runScript s = do
    e <- runEitherT s
    case e of
        Left  m -> fail m
        Right a -> return a


tryReadIORef :: IORef a -> EitherT String IO a
tryReadIORef = scriptIO . readIORef


tryWriteIORef :: Control.Monad.IO.Class.MonadIO m  => IORef a -> a -> EitherT String m ()
tryWriteIORef ref v = scriptIO $ writeIORef ref v


(<??>) :: Monad m => Maybe a -> e -> EitherT e m a
v <??> m = tryRight $ note m v


(<?>) :: Maybe b -> a -> Either a b
v <?> m = note m v


ifnot :: Bool -> String -> Either String ()
ifnot bool msg = if bool 
    then Right ()
    else Left msg


tryGetID :: Monad m => Maybe Int32 -> String -> EitherT String m Int
tryGetID mtID name = do
    tID <- mtID <??> ("'" ++ name  ++ "' argument is missing")
    return $ i32toi tID


tryGetString :: Monad m => Maybe Text -> String -> EitherT String m String
tryGetString mtstring name = do
    tstring <- mtstring <??> ("'" ++ name  ++ "' argument is missing")
    return $ unpack tstring


tryGetUniPath :: Monad m => Maybe Text -> String -> EitherT String m UniPath.UniPath
tryGetUniPath mtpath name = do
    tpath <- mtpath <??> ("'" ++ name  ++ "' argument is missing")
    return $ UniPath.fromUnixString $ unpack tpath

