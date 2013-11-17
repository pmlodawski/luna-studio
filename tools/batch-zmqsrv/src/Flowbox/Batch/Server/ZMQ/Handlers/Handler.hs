
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.Script>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.ZMQ.Handlers.Handler where

import           Flowbox.Control.Error                           
import qualified Generated.Proto.FileSystem.LS.Args            as LS
import qualified Generated.Proto.FileSystem.LS.Result          as LS
import qualified Generated.Proto.FileSystem.Stat.Args          as Stat
import qualified Generated.Proto.FileSystem.Stat.Result        as Stat
import qualified Generated.Proto.FileSystem.MkDir.Args         as MkDir
import qualified Generated.Proto.FileSystem.MkDir.Result       as MkDir
import qualified Generated.Proto.FileSystem.Touch.Args         as Touch
import qualified Generated.Proto.FileSystem.Touch.Result       as Touch
import qualified Generated.Proto.FileSystem.RM.Args            as RM
import qualified Generated.Proto.FileSystem.RM.Result          as RM
import qualified Generated.Proto.FileSystem.CP.Args            as CP
import qualified Generated.Proto.FileSystem.CP.Result          as CP
import qualified Generated.Proto.FileSystem.MV.Args            as MV
import qualified Generated.Proto.FileSystem.MV.Result          as MV
import qualified Generated.Proto.Maintenance.Initialize.Args   as Initialize
import qualified Generated.Proto.Maintenance.Initialize.Result as Initialize
import qualified Generated.Proto.Maintenance.Ping.Args         as Ping
import qualified Generated.Proto.Maintenance.Ping.Result       as Ping
import qualified Generated.Proto.Maintenance.Dump.Args         as Dump
import qualified Generated.Proto.Maintenance.Dump.Result       as Dump
import qualified Generated.Proto.Maintenance.Shutdown.Args     as Shutdown
import qualified Generated.Proto.Maintenance.Shutdown.Result   as Shutdown


class Handler h where
    ls         :: h -> LS.Args    -> Script LS.Result
    stat       :: h -> Stat.Args  -> Script Stat.Result
    mkdir      :: h -> MkDir.Args -> Script MkDir.Result
    touch      :: h -> Touch.Args -> Script Touch.Result
    rm         :: h -> RM.Args    -> Script RM.Result
    cp         :: h -> CP.Args    -> Script CP.Result
    mv         :: h -> MV.Args    -> Script MV.Result
    
    initialize :: h -> Initialize.Args -> Script Initialize.Result
    ping       :: h -> Ping.Args       -> Script Ping.Result
    dump       :: h -> Dump.Args       -> Script Dump.Result
    shutdown   :: h -> Shutdown.Args   -> Script Shutdown.Result
    