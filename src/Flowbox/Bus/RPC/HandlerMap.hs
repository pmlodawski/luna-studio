---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
-- [MW] TODO: review extentions above

module Flowbox.Bus.RPC.HandlerMap (
    module X,
    BatchWriter,
    Callback,
    HandlerMap,
    lookupAndCall,
    makeHandlerMap,
    mkTopic,
    runBatchT,
    runBatchWriter,
    tellUpdate,
    topics
) where

import           Control.Monad.State        (MonadState)
import           Control.Monad.Trans.State  (StateT)
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import           Control.Monad.Writer       (MonadWriter)
import           Data.Binary                (Binary)
import           Data.Map                   as X
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)

import Flowbox.Control.Error
import Flowbox.Bus.Data.Message  (CorrelationID)
import Flowbox.Bus.RPC.RPC       (RPC)
import Flowbox.Bus.RPC.Types
import Flowbox.Prelude           hiding (error)
import Flowbox.System.Log.Logger
import Language.Haskell.TH



logger :: LoggerIO
logger = getLoggerIO $moduleName


newtype BatchT s m a = BatchT { runBatchT :: m a }
                     deriving (Monad, MonadIO, MonadState s, Functor, Applicative)

deriving instance MonadWriter [Value] m => MonadWriter [Value] (BatchT s m)

instance MonadTrans (BatchT s) where
    lift = BatchT

type BatchWriter s m a = BatchT s (WriterT [Value] (EitherT String (StateT s m))) a


type Callback s m = (Binary arg, Typeable arg, Binary res, Typeable res) => (CorrelationID -> arg -> RPC s m (res, [Value])) -> StateT s m (Result, [Value])


type HandlerMap s m = Callback s m -> Map FunctionName (StateT s m (Result, [Value]))


lookupAndCall :: MonadIO m => HandlerMap s m -> Callback s m -> FunctionName -> StateT s m (Result, [Value])
lookupAndCall handlerMap callback functionName = fromMaybe errorHandler $ Map.lookup functionName $ handlerMap callback
    where errorHandler = do let errMsg = "Unknown function: " ++ show functionName
                            logger error errMsg
                            return (ErrorResult errMsg, [])


runBatchWriter :: (Binary arg, Typeable arg, Binary res, Typeable res)
               => Callback s m -> (CorrelationID -> arg -> BatchWriter s m res) -> StateT s m (Result, [Value])
runBatchWriter callback method = callback (\cid arg -> runWriterT $ runBatchT $ method cid arg)


topics :: Monad m => String -> HandlerMap s m -> [FunctionName]
topics pluginName handlerMap = Flowbox.Prelude.map (mkTopic pluginName) $ Map.keys $ handlerMap emptyCallback
    where
        emptyCallback :: Monad m => Callback s m
        emptyCallback = const $ return (ErrorResult "", [])


mkTopic :: String -> String -> String
mkTopic pluginName = ((pluginName <> ".") <>)


makeFunction :: Name -> Type -> [Pat] -> Exp -> [Dec] -> [Dec]
makeFunction fname ftype args bodyExp whereDecs = [prototype, definition] where
    prototype = SigD fname ftype
    definition = FunD fname [Clause args (NormalB bodyExp) whereDecs]


makeHandlerMap :: Name -> Name -> [Name] -> Q [Dec]
makeHandlerMap converter stateType functions = do
    let handlerMapName = mkName "handlerMap"
    ftype <- [t| HandlerMap $(conT stateType) IO |]
    callbackName <- newName "callback"
    let args = VarP <$> [callbackName]
    let listElem fname = [e| ( $(stringE $ show fname), $(varE converter) $(varE callbackName) $(varE fname)) |]

    let theList = listE $ listElem <$> functions

    body <- [e| Map.fromList $(theList) |]
    return $ makeFunction handlerMapName ftype args body []


tellUpdate :: (Binary u, Typeable u, Monad m) => u -> BatchWriter s m ()
tellUpdate u = lift $ tell [packValue u]
