---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Flowbox.Bus.Control.Handler.ID where

import           Data.Binary (Binary)
import qualified Data.IORef  as IORef

import           Flowbox.Bus.Control.BusCtx (BusCtx)
import qualified Flowbox.Bus.Control.BusCtx as BusCtx
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.ZMQ.RPC.RPC        (RPC)


logger :: LoggerIO
logger = getLoggerIO $moduleName

-------- public api -------------------------------------------------

data Request = ID_Create
             deriving (Generic, Show, Typeable)

instance Binary Request


create :: BusCtx -> Request -> RPC Int
create ctx ID_Create = do
    logger info "called ID::create"
    let senderID = BusCtx.nextSenderID ctx
    liftIO $ IORef.atomicModifyIORef senderID
                                     (\i -> let newID = i + 1
                                            in (newID, newID))

