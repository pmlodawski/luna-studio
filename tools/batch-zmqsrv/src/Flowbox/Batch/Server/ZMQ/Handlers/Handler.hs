
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.ZMQ.Handlers.Handler where

import qualified System.ZMQ3.Monadic                                 as ZMQ3

import qualified Generated.Proto.Batch.Maintenance.Initialize.Args   as Initialize
import qualified Generated.Proto.Batch.Maintenance.Initialize.Result as Initialize
import qualified Generated.Proto.Batch.Maintenance.Ping.Args         as Ping
import qualified Generated.Proto.Batch.Maintenance.Ping.Result       as Ping
import qualified Generated.Proto.Batch.Maintenance.Dump.Args         as Dump
import qualified Generated.Proto.Batch.Maintenance.Dump.Result       as Dump
import qualified Generated.Proto.Batch.Maintenance.Shutdown.Args     as Shutdown
import qualified Generated.Proto.Batch.Maintenance.Shutdown.Result   as Shutdown


class Handler h where
    initialize :: h -> Initialize.Args -> ZMQ3.ZMQ z Initialize.Result
    ping       :: h -> Ping.Args       -> ZMQ3.ZMQ z Ping.Result
    dump       :: h -> Dump.Args       -> ZMQ3.ZMQ z Dump.Result
    shutdown   :: h -> Shutdown.Args   -> ZMQ3.ZMQ z Shutdown.Result
