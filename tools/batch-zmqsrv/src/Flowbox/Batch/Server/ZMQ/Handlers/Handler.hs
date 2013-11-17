
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.Script>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.ZMQ.Handlers.Handler where

import           Flowbox.Control.Error                              
import qualified Generated.Proto.FileSystemAPI.LS.Args            as LS
import qualified Generated.Proto.FileSystemAPI.LS.Result          as LS
import qualified Generated.Proto.FileSystemAPI.Stat.Args          as Stat
import qualified Generated.Proto.FileSystemAPI.Stat.Result        as Stat
import qualified Generated.Proto.FileSystemAPI.MkDir.Args         as MkDir
import qualified Generated.Proto.FileSystemAPI.MkDir.Result       as MkDir
import qualified Generated.Proto.FileSystemAPI.Touch.Args         as Touch
import qualified Generated.Proto.FileSystemAPI.Touch.Result       as Touch
import qualified Generated.Proto.FileSystemAPI.RM.Args            as RM
import qualified Generated.Proto.FileSystemAPI.RM.Result          as RM
import qualified Generated.Proto.FileSystemAPI.CP.Args            as CP
import qualified Generated.Proto.FileSystemAPI.CP.Result          as CP
import qualified Generated.Proto.FileSystemAPI.MV.Args            as MV
import qualified Generated.Proto.FileSystemAPI.MV.Result          as MV
import qualified Generated.Proto.ProjectAPI.Projects.Args         as Projects
import qualified Generated.Proto.ProjectAPI.Projects.Result       as Projects
import qualified Generated.Proto.ProjectAPI.ProjectByID.Args      as ProjectByID
import qualified Generated.Proto.ProjectAPI.ProjectByID.Result    as ProjectByID
import qualified Generated.Proto.ProjectAPI.CreateProject.Args    as CreateProject
import qualified Generated.Proto.ProjectAPI.CreateProject.Result  as CreateProject
import qualified Generated.Proto.ProjectAPI.OpenProject.Args      as OpenProject
import qualified Generated.Proto.ProjectAPI.OpenProject.Result    as OpenProject
import qualified Generated.Proto.ProjectAPI.UpdateProject.Args    as UpdateProject
import qualified Generated.Proto.ProjectAPI.UpdateProject.Result  as UpdateProject
import qualified Generated.Proto.ProjectAPI.CloseProject.Args     as CloseProject
import qualified Generated.Proto.ProjectAPI.CloseProject.Result   as CloseProject
import qualified Generated.Proto.ProjectAPI.StoreProject.Args     as StoreProject
import qualified Generated.Proto.ProjectAPI.StoreProject.Result   as StoreProject
import qualified Generated.Proto.MaintenanceAPI.Initialize.Args   as Initialize
import qualified Generated.Proto.MaintenanceAPI.Initialize.Result as Initialize
import qualified Generated.Proto.MaintenanceAPI.Ping.Args         as Ping
import qualified Generated.Proto.MaintenanceAPI.Ping.Result       as Ping
import qualified Generated.Proto.MaintenanceAPI.Dump.Args         as Dump
import qualified Generated.Proto.MaintenanceAPI.Dump.Result       as Dump
import qualified Generated.Proto.MaintenanceAPI.Shutdown.Args     as Shutdown
import qualified Generated.Proto.MaintenanceAPI.Shutdown.Result   as Shutdown


class Handler h where
    ls         :: h -> LS.Args    -> Script LS.Result
    stat       :: h -> Stat.Args  -> Script Stat.Result
    mkdir      :: h -> MkDir.Args -> Script MkDir.Result
    touch      :: h -> Touch.Args -> Script Touch.Result
    rm         :: h -> RM.Args    -> Script RM.Result
    cp         :: h -> CP.Args    -> Script CP.Result
    mv         :: h -> MV.Args    -> Script MV.Result
    
    projects      :: h -> Projects.Args      -> Script Projects.Result
    projectByID   :: h -> ProjectByID.Args   -> Script ProjectByID.Result
    createProject :: h -> CreateProject.Args -> Script CreateProject.Result
    openProject   :: h -> OpenProject.Args   -> Script OpenProject.Result
    updateProject :: h -> UpdateProject.Args -> Script UpdateProject.Result
    closeProject  :: h -> CloseProject.Args  -> Script CloseProject.Result
    storeProject  :: h -> StoreProject.Args  -> Script StoreProject.Result
    
    initialize :: h -> Initialize.Args -> Script Initialize.Result
    ping       :: h -> Ping.Args       -> Script Ping.Result
    dump       :: h -> Dump.Args       -> Script Dump.Result
    shutdown   :: h -> Shutdown.Args   -> Script Shutdown.Result
