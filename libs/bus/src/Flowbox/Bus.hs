

-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PackageImports            #-}

import           Control.Monad.IO.Class
import           "mtl" Control.Monad.Reader
import qualified System.ZMQ4.Monadic    as ZMQ


data BusEnv = BusEnv { controlEndPoint :: String
                     , pullEndPoint    :: String
                     , pubEndPoint     :: String
                     } deriving (Read, Show, Eq)


type BusMonad m = (MonadIO m, MonadReader BusEnv m)


runBus :: MonadIO m => ReaderT s m a -> s -> m a
runBus f env = ZMQ.runZMQ $ do
    liftIO $ print "opening"
    out <- runReaderT f env
    liftIO $ print "closing"
    return out


--getSenderID ::
--getSenderID


incBus = do
    x <- ask
    return ()

test :: BusMonad m => m ()
test = do
    liftIO $ print "dupa"
    x <- ask
    liftIO $ print x
    incBus
    incBus
    incBus
    incBus

main = do
    print =<< runBus test (BusEnv "" "" "")
