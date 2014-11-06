---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.FileManager.RPC.Handler.FileManager where

import           Flowbox.Bus.RPC.RPC                                  (RPC)
import           Flowbox.Prelude                                      hiding (Context, error, id)
import           Flowbox.System.Log.Logger
import qualified Generated.Proto.FileManager.FileManager.Ping.Request as Ping
import qualified Generated.Proto.FileManager.FileManager.Ping.Status  as Ping



logger :: LoggerIO
logger = getLoggerIO $(moduleName)

-------- public api -------------------------------------------------

ping :: Ping.Request -> RPC () IO Ping.Status
ping request = do
    logger info "Ping received"
    return $ Ping.Status request
