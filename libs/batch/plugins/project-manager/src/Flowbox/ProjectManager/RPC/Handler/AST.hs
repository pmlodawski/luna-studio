---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.ProjectManager.RPC.Handler.AST where

import qualified Data.Bimap                                                                           as Bimap
import qualified Data.Sequence                                                                        as Sequence

import qualified Flowbox.Batch.Batch                                                                  as Batch
import qualified Flowbox.Batch.Handler.AST                                                            as BatchAST
import qualified Flowbox.Batch.Handler.Common                                                         as Batch
import qualified Flowbox.Batch.Handler.Properties                                                     as BatchP
import           Flowbox.Bus.Data.Message                                                             (Message)
import           Flowbox.Bus.Data.Topic                                                               (Topic)
import           Flowbox.Bus.RPC.RPC                                                                  (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                                                      hiding (Context, cons)
import           Flowbox.ProjectManager.Context                                                       (Context)
import qualified Flowbox.ProjectManager.RPC.Topic                                                     as Topic
import           Flowbox.System.Log.Logger
import           Flowbox.UR.Manager.Utils                                                             (makeMsgArr, prepareResponse,
                                                                                                       serialize)
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Get.Request                  as CodeGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Get.Status                   as CodeGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Set.Request                  as CodeSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Set.Update                   as CodeSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Request                  as AddData
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Update                   as AddData
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Add.Request             as AddDataCon
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Add.Update              as AddDataCon
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Delete.Request          as DeleteDataCon
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Delete.Update           as DeleteDataCon
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Add.Request       as AddDataConField
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Add.Update        as AddDataConField
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Delete.Request    as DeleteDataConField
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Delete.Update     as DeleteDataConField
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Modify.Request    as ModifyDataConField
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Modify.Update     as ModifyDataConField
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Modify.Request          as ModifyDataCon
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Modify.Update           as ModifyDataCon
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Classes.Request       as ModifyDataClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Classes.Update        as ModifyDataClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cls.Request           as ModifyDataCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cls.Update            as ModifyDataCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cons.Request          as ModifyDataCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cons.Update           as ModifyDataCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Methods.Request       as ModifyDataMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Methods.Update        as ModifyDataMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Request              as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Update               as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Inputs.Request    as ModifyFunctionInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Inputs.Update     as ModifyFunctionInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Name.Request      as ModifyFunctionName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Name.Update       as ModifyFunctionName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Output.Request    as ModifyFunctionOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Output.Update     as ModifyFunctionOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Path.Request      as ModifyFunctionPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Path.Update       as ModifyFunctionPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Request                       as Definitions
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Status                        as Definitions
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Request                as AddModule
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Update                 as AddModule
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Cls.Request         as ModifyModuleCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Cls.Update          as ModifyModuleCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Fields.Request      as ModifyModuleFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Fields.Update       as ModifyModuleFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Imports.Request     as ModifyModuleImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Imports.Update      as ModifyModuleImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.TypeAliases.Request as ModifyModuleTypeAliases
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.TypeAliases.Update  as ModifyModuleTypeAliases
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.TypeDefs.Request    as ModifyModuleTypeDefs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.TypeDefs.Update     as ModifyModuleTypeDefs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Set.Request            as SetASTProperties
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Request                    as Remove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Update                     as Remove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Resolve.Request                   as ResolveDefinition
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Resolve.Status                    as ResolveDefinition
import qualified Generated.Proto.Urm.URM.RegisterMultiple.Request                                     as RegisterMultiple
import qualified Luna.DEP.AST.Control.Crumb                                                           as Crumb
import qualified Luna.DEP.AST.Control.Focus                                                           as Focus
import qualified Luna.DEP.AST.Expr                                                                    as Expr
import qualified Luna.DEP.AST.Module                                                                  as Module
import qualified Luna.DEP.AST.Type                                                                    as Type
import           Luna.DEP.Data.Serialize.Proto.Conversion.Crumb                                       ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Expr                                        ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Focus                                       ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Module                                      ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Name                                        ()



logger :: LoggerIO
logger = getLoggerIO $moduleName

updateIdMaps :: Int -> Int -> Batch.BatchEnv -> Batch.BatchEnv
updateIdMaps newID origID = Batch.idMap  %~ Bimap.insert newID origID


originID :: (Num a, Eq a) => a -> a -> a
originID astID newID = case astID of
                        0              -> newID
                        x | x == astID -> astID
--                                        originID = if isJust maybe
--                                                   then astID
--                                                   else Bimap.lookupR astID

-------- public api -------------------------------------------------

get :: Definitions.Request -> RPC Context IO Definitions.Status
get request@(Definitions.Request mtmaxDepth tbc tlibID tprojectID _) = do
    bc  <- decodeE tbc
    let mmaxDepth = fmap decodeP mtmaxDepth
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    focus <- BatchAST.definitions mmaxDepth bc libID projectID
    return $ Definitions.Status request $ encode focus


addModule :: AddModule.Request -> Maybe Topic -> RPC Context IO ([AddModule.Update], [Message])
addModule request@(AddModule.Request tnewModule tbcParent tlibID tprojectID tastID) undoTopic = do
    newModule <- decodeE tnewModule
    bcParent  <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
        astID     = decodeP tastID
    addedModule <- BatchAST.addModule newModule bcParent libID projectID
    let newBC = bcParent ++ [Crumb.Module $ addedModule ^. Module.cls . Type.name]
        newID = addedModule ^. Module.id
        origin = originID astID newID
    Batch.put . updateIdMaps newID origin =<< Batch.get
    logger warning $ "addMDF astID: " <> show astID
    prepareResponse projectID
                    Topic.projectLibraryAstRemoveRequest
                    (Remove.Request (encode newBC) tlibID tprojectID (encodeP newID))
                    Topic.projectLibraryAstModuleAddRequest
                    request
                    undoTopic
                    "add module"
                    =<< AddModule.Update request (encode addedModule) (encode newBC) <$> Batch.getUpdateNo


addData :: AddData.Request -> Maybe Topic -> RPC Context IO ([AddData.Update], [Message])
addData request@(AddData.Request tnewData tbcParent tlibID tprojectID tastID) undoTopic = do
    newData  <- decodeE tnewData
    bcParent <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
        astID     = decodeP tastID
    addedData <- BatchAST.addData newData bcParent libID projectID
    let newBC = bcParent ++ [Crumb.Class $ addedData ^. Expr.cls . Type.name]
        newID = addedData ^. Expr.id
        origin = originID astID newID
    logger warning $ "addMDF astID: " <> show astID
    Batch.put . updateIdMaps newID origin =<< Batch.get
    prepareResponse projectID
                    Topic.projectLibraryAstRemoveRequest
                    (Remove.Request (encode newBC) tlibID tprojectID (encodeP newID))
                    Topic.projectLibraryAstDataAddRequest
                    request
                    undoTopic
                    "add class"
                    =<< AddData.Update request (encode addedData) (encode newBC) <$> Batch.getUpdateNo


addFunction :: AddFunction.Request -> Maybe Topic -> RPC Context IO ([AddFunction.Update], [Message])
addFunction (AddFunction.Request tnewFunction tbcParent tlibID tprojectID tastID) undoTopic = do
    newFunction <- decodeE tnewFunction
    bcParent    <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
        astID     = decodeP tastID
    addedFunction <- BatchAST.addFunction newFunction bcParent libID projectID
    let newBC     = bcParent ++ [Crumb.Function (addedFunction ^?! Expr.fname) (addedFunction ^. Expr.path)]
        newID     = addedFunction ^. Expr.id
        origin    = originID astID newID
    Batch.put . updateIdMaps newID origin =<< Batch.get
    logger warning $ "addMDF astID: " <> show astID <> ", newID: " <> show newID
    prepareResponse projectID
                    Topic.projectLibraryAstRemoveRequest
                    (Remove.Request (encode newBC) tlibID tprojectID (encodeP origin))
                    Topic.projectLibraryAstFunctionAddRequest
                    (AddFunction.Request tnewFunction tbcParent tlibID tprojectID $ encodeP origin)
                    undoTopic
                    "add function"
                    =<< AddFunction.Update (AddFunction.Request tnewFunction tbcParent tlibID tprojectID (encodeP newID)) (encode addedFunction) (encode newBC) <$> Batch.getUpdateNo


remove :: Remove.Request -> Maybe Topic -> RPC Context IO ([Remove.Update], [Message])
remove request@(Remove.Request tbc tlibID tprojectID tastID) undoTopic = do
    bc  <- decodeE tbc
    context <- Batch.get
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
        tbcParent = (encode $ init bc)
        astID     = decodeP tastID
        originID  = maybe tastID
                          encodeP
                          $ Bimap.lookup astID $ context ^. Batch.idMap
        newID     = maybe tastID
                          encodeP
                          $ Bimap.lookupR astID $ context ^. Batch.idMap
--                                          oldMap                 = context ^. Batch.astMap
--                                          (mOriginId, newAstMap) = Map.insertLookupWithKey (const $ const id) newBC newID oldMap
--                                          originID               = fromMaybe newID mOriginId
    definition <- BatchAST.definitions Nothing bc libID projectID
    nodeID <- Focus.traverseM_ (return . (^?! Module.id)) (return . (^?! Expr.id)) definition
    properties <- BatchP.getProperties nodeID libID projectID
    let tproperties = serialize ("undone." <> Topic.projectLibraryAstPropertiesSetRequest) $ SetASTProperties.Request (encode properties) (encodeP nodeID) tlibID tprojectID
    BatchAST.remove bc libID projectID
    updateNo <- Batch.getUpdateNo
    logger warning $ "rm astID " <> show astID <> ", origin: " <> show originID
    let undoMsg = Sequence.fromList
                     [ case definition of
                         Focus.Function f -> serialize ("undone." <> Topic.projectLibraryAstFunctionAddRequest) $ AddFunction.Request (encode f) tbcParent tlibID tprojectID originID
                         Focus.Class    c -> serialize ("undone." <> Topic.projectLibraryAstDataAddRequest)     $ AddData.Request     (encode c) tbcParent tlibID tprojectID originID
                         Focus.Module   m -> serialize ("undone." <> Topic.projectLibraryAstModuleAddRequest)   $ AddModule.Request   (encode m) tbcParent tlibID tprojectID originID
                     , tproperties
                     ]
    return ( [Remove.Update (Remove.Request tbc tlibID tprojectID newID) updateNo]
           , makeMsgArr (RegisterMultiple.Request
                            undoMsg
                            (serialize ("undone." <> Topic.projectLibraryAstRemoveRequest) $ request)
                            tprojectID
                            (encodeP $ "remove sth")
                        ) undoTopic
           )


resolve :: ResolveDefinition.Request -> RPC Context IO ResolveDefinition.Status
resolve request@(ResolveDefinition.Request tname tbc tlibID tprojectID _) = do
    bc  <- decodeE tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    results <- BatchAST.resolveDefinition name bc libID projectID
    return $ ResolveDefinition.Status request (encode results)


modifyModuleCls :: ModifyModuleCls.Request -> RPC Context IO ModifyModuleCls.Update
modifyModuleCls request@(ModifyModuleCls.Request tcls tbc tlibID tprojectID _) = do
    cls <- decodeE tcls
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyModuleCls cls bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleCls.Update request updateNo


modifyModuleImports :: ModifyModuleImports.Request -> RPC Context IO ModifyModuleImports.Update
modifyModuleImports request@(ModifyModuleImports.Request timports tbc tlibID tprojectID _) = do
    imports <- decodeE timports
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyModuleImports imports bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleImports.Update request updateNo


modifyModuleTypeAliases :: ModifyModuleTypeAliases.Request -> RPC Context IO ModifyModuleTypeAliases.Update
modifyModuleTypeAliases request@(ModifyModuleTypeAliases.Request tfields tbc tlibID tprojectID _) = do
    fields <- decodeE tfields
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyModuleFields fields bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleTypeAliases.Update request updateNo


modifyModuleTypeDefs :: ModifyModuleTypeDefs.Request -> RPC Context IO ModifyModuleTypeDefs.Update
modifyModuleTypeDefs request@(ModifyModuleTypeDefs.Request tfields tbc tlibID tprojectID _) = do
    fields <- decodeE tfields
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyModuleFields fields bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleTypeDefs.Update request updateNo


modifyModuleFields :: ModifyModuleFields.Request -> RPC Context IO ModifyModuleFields.Update
modifyModuleFields request@(ModifyModuleFields.Request tfields tbc tlibID tprojectID _) = do
    fields <- decodeE tfields
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyModuleFields fields bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleFields.Update request updateNo


modifyDataCls :: ModifyDataCls.Request -> Maybe Topic -> RPC Context IO ([ModifyDataCls.Update], [Message])
modifyDataCls request@(ModifyDataCls.Request tcls tbc tlibID tprojectID tastID) undoTopic = do
    cls <- decodeE tcls
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
        toldBC    = encode $ init bc ++ [Crumb.Class $ cls ^?! Type.name]
    toldCls <- encode . (^?! Expr.cls) . (^?! Focus.expr) <$> BatchAST.definitions (Just 1) bc libID projectID
    BatchAST.modifyDataCls cls bc libID projectID
    prepareResponse projectID
                    Topic.projectLibraryAstDataModifyClsRequest
                    (ModifyDataCls.Request toldCls toldBC tlibID tprojectID tastID)
                    Topic.projectLibraryAstDataModifyClsRequest
                    request
                    undoTopic
                    "modify class"
                    =<< ModifyDataCls.Update request <$> Batch.getUpdateNo


modifyDataCon :: ModifyDataCon.Request -> RPC Context IO ModifyDataCon.Update
modifyDataCon request@(ModifyDataCon.Request tcon tconID tbc tlibID tprojectID _) = do
    con <- decodeE tcon
    let conID = decodeP tconID
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyDataCon con conID bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataCon.Update request updateNo


addDataCon :: AddDataCon.Request -> RPC Context IO AddDataCon.Update
addDataCon request@(AddDataCon.Request tcon tbc tlibID tprojectID _) = do
    con <- decodeE tcon
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    conID <- BatchAST.addDataCon con bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ AddDataCon.Update request (encodeP conID) updateNo


deleteDataCon :: DeleteDataCon.Request -> RPC Context IO DeleteDataCon.Update
deleteDataCon request@(DeleteDataCon.Request tconID tbc tlibID tprojectID _) = do
    let conID = decodeP tconID
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.deleteDataCon conID bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ DeleteDataCon.Update request updateNo

addDataConField :: AddDataConField.Request -> RPC Context IO AddDataConField.Update
addDataConField request@(AddDataConField.Request tfield tconID tbc tlibID tprojectID _) = do
    field <- decodeE tfield
    let conID = decodeP tconID
    bc <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    fieldID <- BatchAST.addDataConField field conID bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ AddDataConField.Update request (encodeP fieldID) updateNo

deleteDataConField :: DeleteDataConField.Request -> RPC Context IO DeleteDataConField.Update
deleteDataConField request@(DeleteDataConField.Request tfieldID tconID tbc tlibID tprojectID _) = do
    let fieldID = decodeP tfieldID
        conID = decodeP tconID
    bc <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.deleteDataConField fieldID conID bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ DeleteDataConField.Update request updateNo

modifyDataConField :: ModifyDataConField.Request -> RPC Context IO ModifyDataConField.Update
modifyDataConField request@(ModifyDataConField.Request tfield tfieldID tconID tbc tlibID tprojectID _) = do
    field <- decodeE tfield
    let fieldID = decodeP tfieldID
        conID = decodeP tconID
    bc <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyDataConField field fieldID conID bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataConField.Update request updateNo


modifyDataCons :: ModifyDataCons.Request -> RPC Context IO ModifyDataCons.Update
modifyDataCons request@(ModifyDataCons.Request tcons tbc tlibID tprojectID _) = do
    cons <- decodeE tcons
    bc   <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyDataCons cons bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataCons.Update request updateNo


modifyDataClasses :: ModifyDataClasses.Request -> RPC Context IO ModifyDataClasses.Update
modifyDataClasses request@(ModifyDataClasses.Request tclasses tbc tlibID tprojectID _) = do
    classes <- decodeE tclasses
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyDataClasses classes bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataClasses.Update request updateNo


modifyDataMethods :: ModifyDataMethods.Request -> RPC Context IO ModifyDataMethods.Update
modifyDataMethods request@(ModifyDataMethods.Request tmethods tbc tlibID tprojectID _) = do
    methods <- decodeE tmethods
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyDataMethods methods bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataMethods.Update request updateNo


modifyFunctionName :: ModifyFunctionName.Request -> Maybe Topic -> RPC Context IO ([ModifyFunctionName.Update], [Message])
modifyFunctionName request@(ModifyFunctionName.Request tname tbc tlibID tprojectID tastID) undoTopic = do
    bc <- decodeE tbc
    name <- decodeE tname
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    focus <- Batch.getFunction bc libID projectID
    let tnewBC    = encode $ init bc ++ [Crumb.Function name $ focus ^. Expr.path]
    BatchAST.modifyFunctionName name bc libID projectID
    prepareResponse projectID
                    Topic.projectLibraryAstFunctionModifyNameRequest
                    (ModifyFunctionName.Request (encode $ focus ^?! Expr.fname) tnewBC tlibID tprojectID tastID)
                    Topic.projectLibraryAstFunctionModifyNameRequest
                    request
                    undoTopic
                    ("rename function " ++ (show $ focus ^?! Expr.fname) ++ " to " ++ (show name))
                    =<< ModifyFunctionName.Update request  <$> Batch.getUpdateNo


modifyFunctionPath :: ModifyFunctionPath.Request -> RPC Context IO ModifyFunctionPath.Update
modifyFunctionPath request@(ModifyFunctionPath.Request tpath tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let path      = decodeP tpath
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.modifyFunctionPath path bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyFunctionPath.Update request updateNo


modifyFunctionInputs :: ModifyFunctionInputs.Request -> Maybe Topic -> RPC Context IO ([ModifyFunctionInputs.Update], [Message])
modifyFunctionInputs request@(ModifyFunctionInputs.Request tinputs tbc tlibID tprojectID tastID) undoTopic = do
    inputs <- decodeE tinputs
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    oldInputs <- (^. Expr.inputs) <$> Batch.getFunction bc libID projectID
    BatchAST.modifyFunctionInputs inputs bc libID projectID
    prepareResponse projectID
                    Topic.projectLibraryAstFunctionModifyInputsRequest
                    (ModifyFunctionInputs.Request (encode oldInputs) tbc tlibID tprojectID tastID)
                    Topic.projectLibraryAstFunctionModifyInputsRequest
                    request
                    undoTopic
                    "modify inputs"
                    =<< ModifyFunctionInputs.Update request <$> Batch.getUpdateNo


modifyFunctionOutput :: ModifyFunctionOutput.Request -> Maybe Topic -> RPC Context IO ([ModifyFunctionOutput.Update], [Message])
modifyFunctionOutput request@(ModifyFunctionOutput.Request toutput tbc tlibID tprojectID tastID) undoTopic = do
    output <- decodeE toutput
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    oldOutputs <- (^?! Expr.output) <$> Batch.getFunction bc libID projectID
    BatchAST.modifyFunctionOutput output bc libID projectID
    prepareResponse projectID
                    Topic.projectLibraryAstFunctionModifyOutputRequest
                    (ModifyFunctionOutput.Request (encode oldOutputs) tbc tlibID tprojectID tastID)
                    Topic.projectLibraryAstFunctionModifyOutputRequest
                    request
                    undoTopic
                    ("change output from " ++ (show oldOutputs) ++ " to " ++ (show output))
                    =<< ModifyFunctionOutput.Update request <$> Batch.getUpdateNo


getCode :: CodeGet.Request -> RPC Context IO CodeGet.Status
getCode request@(CodeGet.Request tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    code <- Batch.getCode bc libID projectID
    foldr seq (return ()) code
    return $ CodeGet.Status request $ encodeP code


setCode :: CodeSet.Request -> Maybe Topic -> RPC Context IO ([CodeSet.Update], [Message])
setCode request@(CodeSet.Request tcode tbc tlibID tprojectID tastID) undoTopic = do
    bc <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
        code      = decodeP tcode
    oldCode <- Batch.getCode bc libID projectID
    Batch.setCode code bc libID projectID
    prepareResponse projectID
                    Topic.projectLibraryAstCodeSetRequest
                    (CodeSet.Request (encodeP oldCode) tbc tlibID tprojectID tastID)
                    Topic.projectLibraryAstCodeSetRequest
                    request
                    undoTopic
                    "commit code"
                    =<< (return $ CodeSet.Update request)
