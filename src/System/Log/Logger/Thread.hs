{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Logger.Handler
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Logger.Thread where

import           System.Log.Data               (MonadRecord, appendRecord, Lvl(Lvl), readData, LevelData(LevelData), LookupDataSet)
import           System.Log.Logger.Handler     (MonadLoggerHandler(addHandler))
import           System.Log.Log                (LogFormat, MonadLogger)
import           System.Log.Logger.Base        (BaseLoggerT, runRawBaseLoggerT)
import           System.Log.Logger.Priority    (MonadPriorityLogger(getPriority,setPriority))
import           Control.Monad.Trans           (MonadTrans, lift)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Applicative
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.Reader          (ReaderT, runReaderT)
import           Control.Concurrent.Chan.Unagi (readChan, writeChan, newChan, InChan, OutChan)
import           Control.Exception             (throwIO, catch, SomeException)
import           Control.Concurrent            (forkIO)


----------------------------------------------------------------------
-- ThreadedLogger
----------------------------------------------------------------------

newtype ThreadedLogger' d r m a = ThreadedLogger' { fromThreadedLogger :: ReaderT (InChan (ChMsg d r)) m a } deriving (Monad, MonadIO, Applicative, Functor, MonadTrans)
type ThreadedLogger d m a = ThreadedLogger' d a m a
type instance LogFormat (ThreadedLogger' d r m) = LogFormat m

data ChMsg m a = ChMsg (m ()) | End a | Exc SomeException


runThreadedLogger :: (MonadIO m, Applicative m) => ThreadedLogger m (BaseLoggerT l IO) a -> m a
runThreadedLogger m = do
    (inChan, outChan) <- liftIO newChan
    liftIO $ forkIO $ do
        -- cutting out all the logs and sending them over channel, computing result
        out <- (End <$> (runRawBaseLoggerT $ flip runReaderT inChan . fromThreadedLogger $ m)) `catch` (\e -> return (Exc e))
        writeChan inChan (out)
    loop outChan
    where loop :: (MonadIO m, Applicative m) => OutChan (ChMsg m a) -> m a
          loop ch = do
              l <- liftIO $ readChan ch
              case l of
                  End   a -> return a
                  ChMsg d -> d *> loop ch
                  Exc   e -> liftIO $ throwIO e

getChan = ThreadedLogger' Reader.ask

withTarget f = do
    ch <- getChan
    liftIO $ writeChan ch (ChMsg f)

-- === Instances ===

instance (MonadIO m, MonadRecord d n) => MonadRecord d (ThreadedLogger' n a m) where
    appendRecord = withTarget . appendRecord

instance (MonadIO m, MonadLoggerHandler h d) => MonadLoggerHandler h (ThreadedLogger' d a m) where
    addHandler = withTarget . addHandler

instance (MonadIO m, MonadPriorityLogger d) => MonadPriorityLogger (ThreadedLogger' d a m) where
    setPriority = withTarget . setPriority
    getPriority = error "Cannot get priority from within ThreadLogger!"