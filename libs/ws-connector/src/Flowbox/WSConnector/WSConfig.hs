---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2015
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell           #-}

module Flowbox.WSConnector.WSConfig where

import           Flowbox.Prelude
import qualified FlowboxData.Config.Config  as FD

data Config = Config { _host     :: String
                     , _port     :: Int
                     , _pingTime :: Int
                     } deriving (Read, Show, Eq)

makeLenses ''Config

readWebsocketConfig config = Config host port pingTime where
    host       = FD.host websocket
    port       = read (FD.port websocket) :: Int
    pingTime   = read (FD.pingTime websocket) :: Int
    websocket  = FD.websocket config
