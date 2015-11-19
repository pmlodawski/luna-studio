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

import           Flowbox.Bus.Bus              (Bus)
import qualified Flowbox.Bus.Bus              as Bus
import           Flowbox.Bus.Data.Topic       (Topic)
import qualified Flowbox.Bus.Data.Topic       as Topic
import qualified Flowbox.Bus.RPC.Client       as Client
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger    as L
import qualified Flowbox.Text.ProtocolBuffers as Proto



logger :: LoggerIO
logger = getLoggerIO $moduleName


data Ping = forall arg result. (Proto.Serializable arg, Proto.Serializable result) =>
    Ping { _topic  :: Topic
         , _arg    :: arg
         , _result :: result
         }


exists :: Ping -> Bus Bool
exists (Ping topic request result) = do
    let topicBase = Topic.base topic
    logger info "Testing for duplicates..."
    Bus.withTimeout (Client.query topic request) 1000000 >>= \case
        Left  _ -> Bus.unsubscribe topicBase >> return False
        Right r -> ofSameType r [result] $ return True
    where
        ofSameType :: a -> a -> b -> b
        ofSameType _ _ = id


quitIfExists :: Ping -> Bus ()
quitIfExists ping =
    whenM (exists ping) $ fail "bus plugin already exist, quitting..."
