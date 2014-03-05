---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Broker.Control.Handler.ID where

import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Broker.ID.New.Args             as ID_New
import qualified Generated.Proto.Broker.ID.New.Result           as ID_New



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Broker.Control.Handler.ID"

-------- public api -------------------------------------------------

new :: IORef Int -> ID_New.Args -> IO ID_New.Result
new senderID ID_New.Args = do
    logger info "called newID"
    IORef.atomicModifyIORef senderID (\i -> let newID = i + 1
                                            in (newID, ID_New.Result $ encodeP newID))
