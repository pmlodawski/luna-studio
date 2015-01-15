{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Logger.Base
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Logger.Base where

import Control.Monad.Trans
import System.Log.Data (Data, MonadRecord(appendRecord))
import Control.Applicative
import System.Log.Tuples
import System.Log.Log (LogFormat)

----------------------------------------------------------------------
-- BaseLoggerT
----------------------------------------------------------------------

newtype BaseLoggerT l m a = BaseLoggerT { runRawBaseLoggerT :: m a } deriving (Monad, MonadIO, Applicative, Functor) 

runBaseLoggerT :: (Functor m, Monad m) => l -> BaseLoggerT (MapRTuple Data (Tuple2RTuple l)) m a -> m a
runBaseLoggerT _ = runRawBaseLoggerT

-- === instances ===

type instance LogFormat (BaseLoggerT l m) = l

instance MonadTrans (BaseLoggerT l) where
    lift = BaseLoggerT

instance Monad m => MonadRecord d (BaseLoggerT l m) where
    appendRecord _ = return ()


