
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.Script>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.Handler.Handler where

import           Flowbox.Control.Error
import qualified Generated.Proto.Batch.AST.AddClass.Args                    as AddClass
import qualified Generated.Proto.Batch.AST.AddClass.Result                  as AddClass
import qualified Generated.Proto.Batch.AST.AddFunction.Args                 as AddFunction
import qualified Generated.Proto.Batch.AST.AddFunction.Result               as AddFunction
import qualified Generated.Proto.Batch.AST.AddModule.Args                   as AddModule
import qualified Generated.Proto.Batch.AST.AddModule.Result                 as AddModule
import qualified Generated.Proto.Batch.AST.Definitions.Args                 as Definitions
import qualified Generated.Proto.Batch.AST.Definitions.Result               as Definitions
import qualified Generated.Proto.Batch.AST.Remove.Args                      as Remove
import qualified Generated.Proto.Batch.AST.Remove.Result                    as Remove
import qualified Generated.Proto.Batch.AST.UpdateClassCls.Args              as UpdateClassCls
import qualified Generated.Proto.Batch.AST.UpdateClassCls.Result            as UpdateClassCls
import qualified Generated.Proto.Batch.AST.UpdateClassFields.Args           as UpdateClassFields
import qualified Generated.Proto.Batch.AST.UpdateClassFields.Result         as UpdateClassFields
import qualified Generated.Proto.Batch.AST.UpdateFunctionInputs.Args        as UpdateFunctionInputs
import qualified Generated.Proto.Batch.AST.UpdateFunctionInputs.Result      as UpdateFunctionInputs
import qualified Generated.Proto.Batch.AST.UpdateFunctionName.Args          as UpdateFunctionName
import qualified Generated.Proto.Batch.AST.UpdateFunctionName.Result        as UpdateFunctionName
import qualified Generated.Proto.Batch.AST.UpdateFunctionOutput.Args        as UpdateFunctionOutput
import qualified Generated.Proto.Batch.AST.UpdateFunctionOutput.Result      as UpdateFunctionOutput
import qualified Generated.Proto.Batch.AST.UpdateFunctionPath.Args          as UpdateFunctionPath
import qualified Generated.Proto.Batch.AST.UpdateFunctionPath.Result        as UpdateFunctionPath
import qualified Generated.Proto.Batch.AST.UpdateModuleCls.Args             as UpdateModuleCls
import qualified Generated.Proto.Batch.AST.UpdateModuleCls.Result           as UpdateModuleCls
import qualified Generated.Proto.Batch.AST.UpdateModuleFields.Args          as UpdateModuleFields
import qualified Generated.Proto.Batch.AST.UpdateModuleFields.Result        as UpdateModuleFields
import qualified Generated.Proto.Batch.AST.UpdateModuleImports.Args         as UpdateModuleImports
import qualified Generated.Proto.Batch.AST.UpdateModuleImports.Result       as UpdateModuleImports
import qualified Generated.Proto.Batch.FileSystem.CP.Args                   as CP
import qualified Generated.Proto.Batch.FileSystem.CP.Result                 as CP
import qualified Generated.Proto.Batch.FileSystem.LS.Args                   as LS
import qualified Generated.Proto.Batch.FileSystem.LS.Result                 as LS
import qualified Generated.Proto.Batch.FileSystem.MkDir.Args                as MkDir
import qualified Generated.Proto.Batch.FileSystem.MkDir.Result              as MkDir
import qualified Generated.Proto.Batch.FileSystem.MV.Args                   as MV
import qualified Generated.Proto.Batch.FileSystem.MV.Result                 as MV
import qualified Generated.Proto.Batch.FileSystem.RM.Args                   as RM
import qualified Generated.Proto.Batch.FileSystem.RM.Result                 as RM
import qualified Generated.Proto.Batch.FileSystem.Stat.Args                 as Stat
import qualified Generated.Proto.Batch.FileSystem.Stat.Result               as Stat
import qualified Generated.Proto.Batch.FileSystem.Touch.Args                as Touch
import qualified Generated.Proto.Batch.FileSystem.Touch.Result              as Touch
import qualified Generated.Proto.Batch.Graph.AddNode.Args                   as AddNode
import qualified Generated.Proto.Batch.Graph.AddNode.Result                 as AddNode
import qualified Generated.Proto.Batch.Graph.Connect.Args                   as Connect
import qualified Generated.Proto.Batch.Graph.Connect.Result                 as Connect
import qualified Generated.Proto.Batch.Graph.Disconnect.Args                as Disconnect
import qualified Generated.Proto.Batch.Graph.Disconnect.Result              as Disconnect
import qualified Generated.Proto.Batch.Graph.NodeByID.Args                  as NodeByID
import qualified Generated.Proto.Batch.Graph.NodeByID.Result                as NodeByID
import qualified Generated.Proto.Batch.Graph.NodesGraph.Args                as NodesGraph
import qualified Generated.Proto.Batch.Graph.NodesGraph.Result              as NodesGraph
import qualified Generated.Proto.Batch.Graph.RemoveNode.Args                as RemoveNode
import qualified Generated.Proto.Batch.Graph.RemoveNode.Result              as RemoveNode
import qualified Generated.Proto.Batch.Graph.UpdateNode.Args                as UpdateNode
import qualified Generated.Proto.Batch.Graph.UpdateNode.Result              as UpdateNode
import qualified Generated.Proto.Batch.Library.BuildLibrary.Args            as BuildLibrary
import qualified Generated.Proto.Batch.Library.BuildLibrary.Result          as BuildLibrary
import qualified Generated.Proto.Batch.Library.CreateLibrary.Args           as CreateLibrary
import qualified Generated.Proto.Batch.Library.CreateLibrary.Result         as CreateLibrary
import qualified Generated.Proto.Batch.Library.Libraries.Args               as Libraries
import qualified Generated.Proto.Batch.Library.Libraries.Result             as Libraries
import qualified Generated.Proto.Batch.Library.LibraryByID.Args             as LibraryByID
import qualified Generated.Proto.Batch.Library.LibraryByID.Result           as LibraryByID
import qualified Generated.Proto.Batch.Library.LoadLibrary.Args             as LoadLibrary
import qualified Generated.Proto.Batch.Library.LoadLibrary.Result           as LoadLibrary
import qualified Generated.Proto.Batch.Library.RunLibrary.Args              as RunLibrary
import qualified Generated.Proto.Batch.Library.RunLibrary.Result            as RunLibrary
import qualified Generated.Proto.Batch.Library.StoreLibrary.Args            as StoreLibrary
import qualified Generated.Proto.Batch.Library.StoreLibrary.Result          as StoreLibrary
import qualified Generated.Proto.Batch.Library.UnloadLibrary.Args           as UnloadLibrary
import qualified Generated.Proto.Batch.Library.UnloadLibrary.Result         as UnloadLibrary
import qualified Generated.Proto.Batch.Maintenance.Dump.Args                as Dump
import qualified Generated.Proto.Batch.Maintenance.Dump.Result              as Dump
import qualified Generated.Proto.Batch.Maintenance.Initialize.Args          as Initialize
import qualified Generated.Proto.Batch.Maintenance.Initialize.Result        as Initialize
import qualified Generated.Proto.Batch.Maintenance.Ping.Args                as Ping
import qualified Generated.Proto.Batch.Maintenance.Ping.Result              as Ping
import qualified Generated.Proto.Batch.Maintenance.Shutdown.Args            as Shutdown
import qualified Generated.Proto.Batch.Maintenance.Shutdown.Result          as Shutdown
import qualified Generated.Proto.Batch.NodeDefault.NodeDefaults.Args        as NodeDefaults
import qualified Generated.Proto.Batch.NodeDefault.NodeDefaults.Result      as NodeDefaults
import qualified Generated.Proto.Batch.NodeDefault.RemoveNodeDefault.Args   as RemoveNodeDefault
import qualified Generated.Proto.Batch.NodeDefault.RemoveNodeDefault.Result as RemoveNodeDefault
import qualified Generated.Proto.Batch.NodeDefault.SetNodeDefault.Args      as SetNodeDefault
import qualified Generated.Proto.Batch.NodeDefault.SetNodeDefault.Result    as SetNodeDefault
import qualified Generated.Proto.Batch.Project.CloseProject.Args            as CloseProject
import qualified Generated.Proto.Batch.Project.CloseProject.Result          as CloseProject
import qualified Generated.Proto.Batch.Project.CreateProject.Args           as CreateProject
import qualified Generated.Proto.Batch.Project.CreateProject.Result         as CreateProject
import qualified Generated.Proto.Batch.Project.OpenProject.Args             as OpenProject
import qualified Generated.Proto.Batch.Project.OpenProject.Result           as OpenProject
import qualified Generated.Proto.Batch.Project.ProjectByID.Args             as ProjectByID
import qualified Generated.Proto.Batch.Project.ProjectByID.Result           as ProjectByID
import qualified Generated.Proto.Batch.Project.Projects.Args                as Projects
import qualified Generated.Proto.Batch.Project.Projects.Result              as Projects
import qualified Generated.Proto.Batch.Project.StoreProject.Args            as StoreProject
import qualified Generated.Proto.Batch.Project.StoreProject.Result          as StoreProject
import qualified Generated.Proto.Batch.Project.UpdateProject.Args           as UpdateProject
import qualified Generated.Proto.Batch.Project.UpdateProject.Result         as UpdateProject



class Handler h where
    addModule            :: h -> AddModule.Args            -> Script AddModule.Result
    addClass             :: h -> AddClass.Args             -> Script AddClass.Result
    definitions          :: h -> Definitions.Args             -> Script Definitions.Result
    addFunction          :: h -> AddFunction.Args          -> Script AddFunction.Result
    updateModuleCls      :: h -> UpdateModuleCls.Args      -> Script UpdateModuleCls.Result
    updateModuleImports  :: h -> UpdateModuleImports.Args  -> Script UpdateModuleImports.Result
    updateModuleFields   :: h -> UpdateModuleFields.Args   -> Script UpdateModuleFields.Result
    updateClassCls       :: h -> UpdateClassCls.Args       -> Script UpdateClassCls.Result
    updateClassFields    :: h -> UpdateClassFields.Args    -> Script UpdateClassFields.Result
    updateFunctionName   :: h -> UpdateFunctionName.Args   -> Script UpdateFunctionName.Result
    updateFunctionPath   :: h -> UpdateFunctionPath.Args   -> Script UpdateFunctionPath.Result
    updateFunctionInputs :: h -> UpdateFunctionInputs.Args -> Script UpdateFunctionInputs.Result
    updateFunctionOutput :: h -> UpdateFunctionOutput.Args -> Script UpdateFunctionOutput.Result
    remove               :: h -> Remove.Args               -> Script Remove.Result

    ls         :: h -> LS.Args    -> Script LS.Result
    stat       :: h -> Stat.Args  -> Script Stat.Result
    mkdir      :: h -> MkDir.Args -> Script MkDir.Result
    touch      :: h -> Touch.Args -> Script Touch.Result
    rm         :: h -> RM.Args    -> Script RM.Result
    cp         :: h -> CP.Args    -> Script CP.Result
    mv         :: h -> MV.Args    -> Script MV.Result

    nodesGraph :: h -> NodesGraph.Args -> Script NodesGraph.Result
    nodeByID   :: h -> NodeByID.Args   -> Script NodeByID.Result
    addNode    :: h -> AddNode.Args    -> Script AddNode.Result
    updateNode :: h -> UpdateNode.Args -> Script UpdateNode.Result
    removeNode :: h -> RemoveNode.Args -> Script RemoveNode.Result
    connect    :: h -> Connect.Args    -> Script Connect.Result
    disconnect :: h -> Disconnect.Args -> Script Disconnect.Result

    libraries     :: h -> Libraries.Args     -> Script Libraries.Result
    libraryByID   :: h -> LibraryByID.Args   -> Script LibraryByID.Result
    createLibrary :: h -> CreateLibrary.Args -> Script CreateLibrary.Result
    loadLibrary   :: h -> LoadLibrary.Args   -> Script LoadLibrary.Result
    unloadLibrary :: h -> UnloadLibrary.Args -> Script UnloadLibrary.Result
    storeLibrary  :: h -> StoreLibrary.Args  -> Script StoreLibrary.Result
    buildLibrary  :: h -> BuildLibrary.Args  -> Script BuildLibrary.Result
    runLibrary    :: h -> RunLibrary.Args    -> Script RunLibrary.Result

    initialize :: h -> Initialize.Args -> Script Initialize.Result
    ping       :: h -> Ping.Args       -> Script Ping.Result
    dump       :: h -> Dump.Args       -> Script Dump.Result
    shutdown   :: h -> Shutdown.Args   -> Script Shutdown.Result

    nodeDefaults      :: h -> NodeDefaults.Args      -> Script NodeDefaults.Result
    setNodeDefault    :: h -> SetNodeDefault.Args    -> Script SetNodeDefault.Result
    removeNodeDefault :: h -> RemoveNodeDefault.Args -> Script RemoveNodeDefault.Result

    projects      :: h -> Projects.Args      -> Script Projects.Result
    projectByID   :: h -> ProjectByID.Args   -> Script ProjectByID.Result
    createProject :: h -> CreateProject.Args -> Script CreateProject.Result
    openProject   :: h -> OpenProject.Args   -> Script OpenProject.Result
    updateProject :: h -> UpdateProject.Args -> Script UpdateProject.Result
    closeProject  :: h -> CloseProject.Args  -> Script CloseProject.Result
    storeProject  :: h -> StoreProject.Args  -> Script StoreProject.Result
