
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.IO>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.Handler.Handler where

import           Flowbox.Prelude
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
import qualified Generated.Proto.Batch.AST.ResolveDefinition.Args           as ResolveDefinition
import qualified Generated.Proto.Batch.AST.ResolveDefinition.Result         as ResolveDefinition
import qualified Generated.Proto.Batch.AST.UpdateDataClasses.Args           as UpdateDataClasses
import qualified Generated.Proto.Batch.AST.UpdateDataClasses.Result         as UpdateDataClasses
import qualified Generated.Proto.Batch.AST.UpdateDataCls.Args               as UpdateDataCls
import qualified Generated.Proto.Batch.AST.UpdateDataCls.Result             as UpdateDataCls
import qualified Generated.Proto.Batch.AST.UpdateDataCons.Args              as UpdateDataCons
import qualified Generated.Proto.Batch.AST.UpdateDataCons.Result            as UpdateDataCons
import qualified Generated.Proto.Batch.AST.UpdateDataMethods.Args           as UpdateDataMethods
import qualified Generated.Proto.Batch.AST.UpdateDataMethods.Result         as UpdateDataMethods
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
import qualified Generated.Proto.Batch.Parser.ParseExpr.Args                as ParseExpr
import qualified Generated.Proto.Batch.Parser.ParseExpr.Result              as ParseExpr
import qualified Generated.Proto.Batch.Parser.ParsePat.Args                 as ParsePat
import qualified Generated.Proto.Batch.Parser.ParsePat.Result               as ParsePat
import qualified Generated.Proto.Batch.Parser.ParseType.Args                as ParseType
import qualified Generated.Proto.Batch.Parser.ParseType.Result              as ParseType
import qualified Generated.Proto.Batch.Process.Processes.Args               as Processes
import qualified Generated.Proto.Batch.Process.Processes.Result             as Processes
import qualified Generated.Proto.Batch.Process.Terminate.Args               as Terminate
import qualified Generated.Proto.Batch.Process.Terminate.Result             as Terminate
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
import qualified Generated.Proto.Batch.Properties.GetProperties.Args        as GetProperties
import qualified Generated.Proto.Batch.Properties.GetProperties.Result      as GetProperties
import qualified Generated.Proto.Batch.Properties.SetProperties.Args        as SetProperties
import qualified Generated.Proto.Batch.Properties.SetProperties.Result      as SetProperties



class Handler h where
    addModule            :: h -> AddModule.Args            -> IO AddModule.Result
    addClass             :: h -> AddClass.Args             -> IO AddClass.Result
    addFunction          :: h -> AddFunction.Args          -> IO AddFunction.Result
    definitions          :: h -> Definitions.Args          -> IO Definitions.Result
    updateModuleCls      :: h -> UpdateModuleCls.Args      -> IO UpdateModuleCls.Result
    updateModuleImports  :: h -> UpdateModuleImports.Args  -> IO UpdateModuleImports.Result
    updateModuleFields   :: h -> UpdateModuleFields.Args   -> IO UpdateModuleFields.Result
    updateDataCls        :: h -> UpdateDataCls.Args        -> IO UpdateDataCls.Result
    updateDataCons       :: h -> UpdateDataCons.Args       -> IO UpdateDataCons.Result
    updateDataClasses    :: h -> UpdateDataClasses.Args    -> IO UpdateDataClasses.Result
    updateDataMethods    :: h -> UpdateDataMethods.Args    -> IO UpdateDataMethods.Result
    updateFunctionName   :: h -> UpdateFunctionName.Args   -> IO UpdateFunctionName.Result
    updateFunctionPath   :: h -> UpdateFunctionPath.Args   -> IO UpdateFunctionPath.Result
    updateFunctionInputs :: h -> UpdateFunctionInputs.Args -> IO UpdateFunctionInputs.Result
    updateFunctionOutput :: h -> UpdateFunctionOutput.Args -> IO UpdateFunctionOutput.Result
    remove               :: h -> Remove.Args               -> IO Remove.Result
    resolveDefinition    :: h -> ResolveDefinition.Args    -> IO ResolveDefinition.Result

    ls         :: h -> LS.Args    -> IO LS.Result
    stat       :: h -> Stat.Args  -> IO Stat.Result
    mkdir      :: h -> MkDir.Args -> IO MkDir.Result
    touch      :: h -> Touch.Args -> IO Touch.Result
    rm         :: h -> RM.Args    -> IO RM.Result
    cp         :: h -> CP.Args    -> IO CP.Result
    mv         :: h -> MV.Args    -> IO MV.Result

    nodesGraph :: h -> NodesGraph.Args -> IO NodesGraph.Result
    nodeByID   :: h -> NodeByID.Args   -> IO NodeByID.Result
    addNode    :: h -> AddNode.Args    -> IO AddNode.Result
    updateNode :: h -> UpdateNode.Args -> IO UpdateNode.Result
    removeNode :: h -> RemoveNode.Args -> IO RemoveNode.Result
    connect    :: h -> Connect.Args    -> IO Connect.Result
    disconnect :: h -> Disconnect.Args -> IO Disconnect.Result

    libraries     :: h -> Libraries.Args     -> IO Libraries.Result
    libraryByID   :: h -> LibraryByID.Args   -> IO LibraryByID.Result
    createLibrary :: h -> CreateLibrary.Args -> IO CreateLibrary.Result
    loadLibrary   :: h -> LoadLibrary.Args   -> IO LoadLibrary.Result
    unloadLibrary :: h -> UnloadLibrary.Args -> IO UnloadLibrary.Result
    storeLibrary  :: h -> StoreLibrary.Args  -> IO StoreLibrary.Result
    buildLibrary  :: h -> BuildLibrary.Args  -> IO BuildLibrary.Result
    runLibrary    :: h -> RunLibrary.Args    -> IO RunLibrary.Result

    initialize :: h -> Initialize.Args -> IO Initialize.Result
    ping       :: h -> Ping.Args       -> IO Ping.Result
    dump       :: h -> Dump.Args       -> IO Dump.Result
    shutdown   :: h -> Shutdown.Args   -> IO Shutdown.Result

    nodeDefaults      :: h -> NodeDefaults.Args      -> IO NodeDefaults.Result
    setNodeDefault    :: h -> SetNodeDefault.Args    -> IO SetNodeDefault.Result
    removeNodeDefault :: h -> RemoveNodeDefault.Args -> IO RemoveNodeDefault.Result

    parseExpr :: h -> ParseExpr.Args -> IO ParseExpr.Result
    parsePat  :: h -> ParsePat.Args  -> IO ParsePat.Result
    parseType :: h -> ParseType.Args -> IO ParseType.Result

    processes :: h -> Processes.Args -> IO Processes.Result
    terminate :: h -> Terminate.Args -> IO Terminate.Result

    projects      :: h -> Projects.Args      -> IO Projects.Result
    projectByID   :: h -> ProjectByID.Args   -> IO ProjectByID.Result
    createProject :: h -> CreateProject.Args -> IO CreateProject.Result
    openProject   :: h -> OpenProject.Args   -> IO OpenProject.Result
    updateProject :: h -> UpdateProject.Args -> IO UpdateProject.Result
    closeProject  :: h -> CloseProject.Args  -> IO CloseProject.Result
    storeProject  :: h -> StoreProject.Args  -> IO StoreProject.Result

    getProperties :: h -> GetProperties.Args -> IO GetProperties.Result
    setProperties :: h -> SetProperties.Args -> IO SetProperties.Result
