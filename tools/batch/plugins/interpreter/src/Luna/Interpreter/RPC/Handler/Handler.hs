---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Luna.Interpreter.RPC.Handler.Handler where

import           Control.Monad             (forever)
import           Control.Monad.Trans.State
import           Data.IORef                (IORef)
import qualified Data.IORef                as IORef
import           Pipes                     (liftIO, (>->))
import qualified Pipes
import qualified Pipes.Concurrent          as Pipes

import           Flowbox.Bus.Data.Message                 (Message)
import qualified Flowbox.Bus.Data.Message                 as Message
import           Flowbox.Bus.Data.Prefix                  (Prefix)
import qualified Flowbox.Bus.Data.Prefix                  as Prefix
import           Flowbox.Bus.Data.Topic                   (status, update, (/+))
import qualified Flowbox.Bus.Data.Topic                   as Topic
import           Flowbox.Bus.RPC.HandlerMap               (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap               as HandlerMap
import           Flowbox.Bus.RPC.RPC                      (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor         as Processor
import           Flowbox.Config.Config                    (Config)
import           Flowbox.Prelude                          hiding (Context, error)
import           Flowbox.ProjectManager.Context           (Context)
import qualified Flowbox.ProjectManager.RPC.Topic         as Topic
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers             as Proto
import qualified Luna.Interpreter.RPC.Handler.ASTWatch    as ASTWatch
import qualified Luna.Interpreter.RPC.Handler.Interpreter as Interpreter
import qualified Luna.Interpreter.RPC.Handler.Sync        as Sync
import qualified Luna.Interpreter.RPC.Handler.Value       as Value
import qualified Luna.Interpreter.RPC.Topic               as Topic
import qualified Luna.Interpreter.Session.Env             as Env
import           Luna.Interpreter.Session.Error           (Error)
import           Luna.Interpreter.Session.Session         (SessionST)
import qualified Luna.Interpreter.Session.Session         as Session


logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.RPC.Handler.Handler"


handlerMap :: Prefix -> HandlerMap Context SessionST
handlerMap prefix callback = HandlerMap.fromList $ Prefix.prefixifyTopics prefix
    [ (Topic.interpreterSetProjectIDRequest    , respond Topic.update Interpreter.setProjectID    )
    , (Topic.interpreterGetProjectIDRequest    , respond Topic.update Interpreter.getProjectID    )
    , (Topic.interpreterSetMainPtrRequest      , respond Topic.update Interpreter.setMainPtr      )
    , (Topic.interpreterGetMainPtrRequest      , respond Topic.update Interpreter.getMainPtr      )
    , (Topic.interpreterRunRequest             , respond Topic.update Interpreter.run             )
    , (Topic.interpreterWatchPointAddRequest   , respond Topic.update Interpreter.watchPointAdd   )
    , (Topic.interpreterWatchPointRemoveRequest, respond Topic.update Interpreter.watchPointRemove)
    , (Topic.interpreterWatchPointListRequest  , respond Topic.status Interpreter.watchPointList  )
    , (Topic.interpreterValueRequest           , respond Topic.update Value.get                   )
    , (Topic.interpreterPingRequest            , respond Topic.update Interpreter.ping            )

    , (Topic.projectmanagerSyncGetRequest                           /+ status, call0 Sync.projectmanagerSyncGet)

    , (Topic.projectCreateRequest                                   /+ update, optionalSync ASTWatch.projectCreate)
    , (Topic.projectOpenRequest                                     /+ update, requiredSync ASTWatch.projectOpen)
    , (Topic.projectCloseRequest                                    /+ update, optionalSync ASTWatch.projectClose)
    , (Topic.projectModifyRequest                                   /+ update, optionalSync ASTWatch.projectModify)
    , (Topic.projectLibraryCreateRequest                            /+ update, optionalSync ASTWatch.libraryCreate)
    , (Topic.projectLibraryLoadRequest                              /+ update, requiredSync ASTWatch.libraryLoad)
    , (Topic.projectLibraryUnloadRequest                            /+ update, optionalSync ASTWatch.libraryUnload)
    , (Topic.projectLibraryAstRemoveRequest                         /+ update, optionalSync ASTWatch.astRemove)
    , (Topic.projectLibraryAstModuleAddRequest                      /+ update, optionalSync ASTWatch.astModuleAdd)
    , (Topic.projectLibraryAstModuleModifyClsRequest                /+ update, optionalSync ASTWatch.astModuleModifyCls)
    , (Topic.projectLibraryAstModuleModifyFieldsRequest             /+ update, optionalSync ASTWatch.astModuleModifyFields)
    , (Topic.projectLibraryAstModuleModifyImportsRequest            /+ update, optionalSync ASTWatch.astModuleModifyImports)
    , (Topic.projectLibraryAstDataAddRequest                        /+ update, optionalSync ASTWatch.astDataAdd)
    , (Topic.projectLibraryAstDataModifyClassesRequest              /+ update, optionalSync ASTWatch.astDataModifyClasses)
    , (Topic.projectLibraryAstDataModifyClsRequest                  /+ update, optionalSync ASTWatch.astDataModifyCls)
    , (Topic.projectLibraryAstDataModifyConsRequest                 /+ update, optionalSync ASTWatch.astDataModifyCons)
    , (Topic.projectLibraryAstDataModifyMethodsRequest              /+ update, optionalSync ASTWatch.astDataModifyMethods)
    , (Topic.projectLibraryAstFunctionAddRequest                    /+ update, optionalSync ASTWatch.astFunctionAdd)
    , (Topic.projectLibraryAstFunctionModifyInputsRequest           /+ update, optionalSync ASTWatch.astFunctionModifyInputs)
    , (Topic.projectLibraryAstFunctionModifyNameRequest             /+ update, optionalSync ASTWatch.astFunctionModifyName)
    , (Topic.projectLibraryAstFunctionModifyOutputRequest           /+ update, optionalSync ASTWatch.astFunctionModifyOutput)
    , (Topic.projectLibraryAstFunctionModifyPathRequest             /+ update, optionalSync ASTWatch.astFunctionModifyPath)
    , (Topic.projectLibraryAstPropertiesGetRequest                  /+ update, optionalSync ASTWatch.astPropertiesSet)
    , (Topic.projectLibraryAstFunctionGraphConnectRequest           /+ update, optionalSync ASTWatch.graphConnect)
    , (Topic.projectLibraryAstFunctionGraphDisconnectRequest        /+ update, optionalSync ASTWatch.graphDisconnect)
    , (Topic.projectLibraryAstFunctionGraphNodeAddRequest           /+ update, optionalSync ASTWatch.graphNodeAdd)
    , (Topic.projectLibraryAstFunctionGraphNodeRemoveRequest        /+ update, optionalSync ASTWatch.graphNodeRemove)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyRequest        /+ update, optionalSync ASTWatch.graphNodeModify)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest /+ update, optionalSync ASTWatch.graphNodeModifyInPlace)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultRemoveRequest /+ update, optionalSync ASTWatch.graphNodeDefaultRemove)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest    /+ update, optionalSync ASTWatch.graphNodeDefaultSet)
    , (Topic.projectLibraryAstFunctionGraphNodePropertiesSetRequest /+ update, optionalSync ASTWatch.graphNodePropertiesSet)
    ]
    where
        respond type_ = callback (/+ type_) . Processor.singleResult

        --query topic = callback (const topic) . Processor.singleResult

        call0 :: Proto.Serializable a => (a -> RPC Context SessionST ()) -> StateT Context SessionST [Message]
        call0 = callback id . Processor.noResult

        optionalSync :: Proto.Serializable a => (a -> RPC Context SessionST ()) -> StateT Context SessionST [Message]
        optionalSync = callback (const Topic.projectmanagerSyncGetRequest) . Processor.optResult . (.) Sync.syncIfNeeded

        requiredSync :: Proto.Serializable a => (a -> RPC Context SessionST ()) -> StateT Context SessionST [Message]
        requiredSync fun = callback (const Topic.projectmanagerSyncGetRequest) $ Processor.singleResult (\args -> fun args >> Sync.syncRequest)


extraImports :: [Session.Import]
extraImports = [("FlowboxM.Libs.Flowbox.Std", Nothing)]


interpret :: Prefix
          -> IORef Message.CorrelationID
          -> Pipes.Pipe (Message, Message.CorrelationID)
                        (Message, Message.CorrelationID)
                        (StateT Context SessionST) ()
interpret prefix crlRef = forever $ do
    (message, crl) <- Pipes.await
    liftIO $ IORef.writeIORef crlRef crl
    results <- lift $ Processor.processLifted (handlerMap prefix) message
    mapM_ (\r -> Pipes.yield (r, crl)) results


run :: Config -> Prefix -> Context
    -> (Pipes.Input  (Message, Message.CorrelationID),
        Pipes.Output (Message, Message.CorrelationID))
    -> IO (Either Error ())
run cfg prefix ctx (input, output) = do
    crlRef <- IORef.newIORef def
    let env = def & Env.resultCallBack .~ Value.reportOutputValue crlRef output
    Session.run cfg env extraImports $ lift $ flip evalStateT ctx $
        Pipes.runEffect $ Pipes.fromInput input
                      >-> interpret prefix crlRef
                      >-> Pipes.toOutput output
