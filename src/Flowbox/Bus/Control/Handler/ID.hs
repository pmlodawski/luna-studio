---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Bus.Control.Handler.ID where

import qualified Data.IORef as IORef

import           Flowbox.Bus.Control.BusCtx                     (BusCtx)
import qualified Flowbox.Bus.Control.BusCtx                     as BusCtx
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import           Flowbox.ZMQ.RPC.RPC                            (RPC, liftIO)
import qualified Generated.Proto.Bus.ID.Create.Args             as ID_Create
import qualified Generated.Proto.Bus.ID.Create.Result           as ID_Create


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.Control.Handler.ID"

-------- public api -------------------------------------------------

create :: BusCtx -> ID_Create.Args -> RPC ID_Create.Result
create ctx ID_Create.Args = do
    logger info "called ID::create"
    let senderID = BusCtx.nextSenderID ctx
    liftIO $ IORef.atomicModifyIORef senderID
                                     (\i -> let newID = i + 1
                                            in (newID, ID_Create.Result $ encodeP newID))

