---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

module Flowbox.Bus.Util where

import Data.Binary (Binary)

import           Flowbox.Bus.Bus              (Bus)
import qualified Flowbox.Bus.Bus              as Bus
import           Flowbox.Bus.Data.Topic       (Topic)
import qualified Flowbox.Bus.Data.Topic       as Topic
import qualified Flowbox.Bus.RPC.Client       as Client
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger    as L

import Debug.Trace as T


logger :: LoggerIO
logger = getLoggerIO $moduleName


data Ping = forall arg result. (Typeable arg, Binary arg, Typeable result, Binary result) =>
    Ping { _topic  :: Topic
         , _arg    :: arg
         , _result :: result
         }


exists :: String -> Ping -> Bus Bool
exists pluginName (Ping topic request result) = do
    let topicBase = Topic.base topic
    logger info "Testing for duplicates..."
    Bus.withTimeout (Client.query pluginName topic request) 1000000 >>= \case
        Left  _ -> do
            T.trace "KD!!" $ return ()
            Bus.unsubscribe topicBase >> return False
        Right r -> do
            T.trace "DK" $ return ()
            ofSameType r [result] $ return True
    where
        ofSameType :: a -> a -> b -> b
        ofSameType _ _ = id


quitIfExists :: String -> Ping -> Bus ()
quitIfExists pluginName ping =
    whenM (exists pluginName ping) $ fail "bus plugin already exist, quitting..."
