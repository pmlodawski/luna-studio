---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Luna.Interpreter.RPC.Handler.Handler where

import           Control.Monad.Trans.State
import           Data.IORef                                  (IORef)
import qualified Data.IORef                                  as IORef
import           Pipes                                       ((>->))
import qualified Pipes
import qualified Pipes.Concurrent                            as Pipes
import qualified Pipes.Safe                                  as Pipes

import           Flowbox.Bus.Data.Flag                       (Flag)
import qualified Flowbox.Bus.Data.Flag                       as Flag
import           Flowbox.Bus.Data.Message                    (Message)
import qualified Flowbox.Bus.Data.Message                    as Message
import           Flowbox.Bus.Data.Prefix                     (Prefix)
import qualified Flowbox.Bus.Data.Prefix                     as Prefix
import           Flowbox.Bus.Data.Topic                      (Topic, status, update, (/+))
import           Flowbox.Bus.RPC.HandlerMap                  (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                  as HandlerMap
import           Flowbox.Bus.RPC.RPC                         (RPC)
import qualified Flowbox.Bus.RPC.RPC                         as RPC
import qualified Flowbox.Bus.RPC.Server.Processor            as Processor
import           Flowbox.Config.Config                       (Config)
import qualified Flowbox.Control.Concurrent                  as Concurrent
import           Flowbox.Control.Monad.Loops                 (untilTrue)
import           Flowbox.Prelude                             hiding (Context, error)
import           Flowbox.ProjectManager.Context              (Context)
import qualified Flowbox.ProjectManager.RPC.Topic            as Topic
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                as Proto
import qualified Luna.Interpreter.RPC.Handler.Abort          as Abort
import qualified Luna.Interpreter.RPC.Handler.ASTWatch       as ASTWatch
import qualified Luna.Interpreter.RPC.Handler.Interpreter    as Interpreter
import qualified Luna.Interpreter.RPC.Handler.Preprocess     as Preprocess
import qualified Luna.Interpreter.RPC.Handler.Renderer       as Renderer
import qualified Luna.Interpreter.RPC.Handler.Sync           as Sync
import qualified Luna.Interpreter.RPC.Handler.Value          as Value
import           Luna.Interpreter.RPC.QueueInfo              (QueueInfo)
import qualified Luna.Interpreter.RPC.QueueInfo              as QueueInfo
import qualified Luna.Interpreter.RPC.Topic                  as Topic
import qualified Luna.Interpreter.Session.Env                as Env
import           Luna.Interpreter.Session.Error              (Error)
import           Luna.Interpreter.Session.Memory.Manager     (MemoryManager)
import           Luna.Interpreter.Session.Memory.Manager.LRU (LRU)
import           Luna.Interpreter.Session.Session            (SessionST)
import qualified Luna.Interpreter.Session.Session            as Session



logger :: LoggerIO
logger = getLoggerIO $moduleName


topics :: Prefix -> [Topic]
topics prefix = HandlerMap.topics $ handlerMap prefix undefined undefined undefined


type MM = LRU


handlerMap :: Prefix -> QueueInfo -> Message.CorrelationID
           -> Pipes.Output (Message, Message.CorrelationID, Flag) -> HandlerMap Context (SessionST MM)
handlerMap prefix queueInfo crl output callback = HandlerMap.fromList $ Prefix.prefixifyTopics prefix
    [ (Topic.interpreterSetProjectIDRequest                , respond update   Interpreter.setProjectID     )
    , (Topic.interpreterGetProjectIDRequest                , respond status   Interpreter.getProjectID     )
    , (Topic.interpreterSetMainPtrRequest                  , respond update   Interpreter.setMainPtr       )
    , (Topic.interpreterGetMainPtrRequest                  , respond status   Interpreter.getMainPtr       )
    , (Topic.interpreterRunRequest                         , respond update $ Interpreter.run queueInfo crl)
    , (Topic.interpreterInvalidateRequest                  , respond update   Interpreter.invalidate       )
    , (Topic.interpreterWatchPointAddRequest               , respond update   Interpreter.watchPointAdd    )
    , (Topic.interpreterWatchPointRemoveRequest            , respond update   Interpreter.watchPointRemove )
    , (Topic.interpreterWatchPointListRequest              , respond status   Interpreter.watchPointList   )
    , (Topic.interpreterValueRequest                       , respond update   Value.get                    )
    , (Topic.interpreterPingRequest                        , respond status   Interpreter.ping             )
    , (Topic.interpreterAbortRequest                       , respond status   Interpreter.abort            )
    , (Topic.interpreterVarTimeGetRequest                  , respond status   Interpreter.varTimeGet       )
    , (Topic.interpreterVarTimeSetRequest                  , respond update   Interpreter.varTimeSet       )
    , (Topic.interpreterSerializationModeGetRequest        , respond status   Interpreter.getSerializationMode       )
    , (Topic.interpreterSerializationModeInsertRequest     , respond update   Interpreter.insertSerializationMode    )
    , (Topic.interpreterSerializationModeDeleteRequest     , respond update   Interpreter.deleteSerializationMode    )
    , (Topic.interpreterSerializationModeDeleteAllRequest  , respond update   Interpreter.deleteAllSerializationMode )
    , (Topic.interpreterMemoryGetLimitsRequest             , respond status   Interpreter.getMemoryLimits)
    , (Topic.interpreterMemorySetLimitsRequest             , respond update   Interpreter.setMemoryLimits)
    , (Topic.interpreterExitRequest                        , respond update   Interpreter.exit)

    , (Topic.rendererRenderRequest     , respond update $ Renderer.render     $ Renderer.reportProgress crl output)
    , (Topic.rendererRenderNodeRequest , respond update $ Renderer.renderNode $ Renderer.reportProgress crl output)

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
    , (Topic.projectLibraryAstDataConModifyRequest                  /+ update, optionalSync ASTWatch.astDataConModify)
    , (Topic.projectLibraryAstDataConAddRequest                     /+ update, optionalSync ASTWatch.astDataConAdd)
    , (Topic.projectLibraryAstDataConDeleteRequest                  /+ update, optionalSync ASTWatch.astDataConDelete)
    , (Topic.projectLibraryAstDataConFieldDeleteRequest             /+ update, optionalSync ASTWatch.astDataConFieldDelete)
    , (Topic.projectLibraryAstDataConFieldAddRequest                /+ update, optionalSync ASTWatch.astDataConFieldAdd)
    , (Topic.projectLibraryAstDataConFieldModifyRequest             /+ update, optionalSync ASTWatch.astDataConFieldModify)
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

        call0 :: Proto.Serializable a
              => (a -> RPC Context (SessionST MM) ()) -> StateT Context (SessionST MM) [Message]
        call0 = callback id . Processor.noResult

        optionalSync :: Proto.Serializable a
                     => (a -> RPC Context (SessionST MM) ()) -> StateT Context (SessionST MM) [Message]
        optionalSync = callback (const Topic.projectmanagerSyncGetRequest) . Processor.optResult . (.) Sync.syncIfNeeded

        requiredSync :: Proto.Serializable a
                     => (a -> RPC Context (SessionST MM) ()) -> StateT Context (SessionST MM) [Message]
        requiredSync fun = callback (const Topic.projectmanagerSyncGetRequest) $ Processor.singleResult (\args -> RPC.interceptErrors (fun args) >> Sync.syncRequest)


extraImports :: [Session.Import]
extraImports = ["FlowboxM.Libs.Flowbox.Std"
               ]


interpret :: MemoryManager MM
          => Prefix -> QueueInfo
          -> IORef Message.CorrelationID
          -> Pipes.Output (Message, Message.CorrelationID, Flag)
          -> Pipes.Pipe (Message, Message.CorrelationID)
                        (Message, Message.CorrelationID, Flag)
                        (Pipes.SafeT (StateT Context (SessionST MM))) ()
interpret prefix queueInfo crlRef output = untilTrue $ do
    (message, crl) <- Pipes.await
    let topic = message ^. Message.topic
    logger trace $ "Received message " ++ topic
    liftIO $ IORef.writeIORef crlRef crl
    results <- lift2 $ Processor.process (handlerMap prefix queueInfo crl output) message
    logger trace $ "Sending " ++ show (length results) ++ " result"
    let send []    = return ()
        send [r]   = Pipes.yield (r, crl, Flag.Enable)
        send (r:t) = Pipes.yield (r, crl, Flag.Disable) >> send t
    send results
    return $ topic == Topic.interpreterExitRequest


run :: Config -> Prefix -> Context
    -> (Pipes.Input  (Message, Message.CorrelationID),
        Pipes.Output (Message, Message.CorrelationID, Flag))
    -> IO (Either Error ())
run cfg prefix ctx (input, output) = do
    crlRef              <- IORef.newIORef def
    interpreterThreadId <- Concurrent.myThreadId
    queueInfo           <- QueueInfo.mk
    (output1, input1)   <- Pipes.spawn Pipes.unbounded
    env <- (Env.sessionConfig . Env.resultCallBack .~ Value.reportOutputValue crlRef output) <$> Env.mkDef cfg def
    Concurrent.forkIO_ $ Pipes.runEffect $
            Pipes.fromInput input
        >-> Preprocess.preprocess prefix queueInfo (env ^. Env.sessionStatus . Env.fragileOperation) interpreterThreadId
        >-> Pipes.toOutput output1
    Session.run cfg env extraImports $ lift $ flip evalStateT ctx $
        Pipes.runSafeT $ Pipes.runEffect $ Abort.handleAbort $
                Pipes.fromInput input1
            >-> interpret prefix queueInfo crlRef output
            >-> Pipes.toOutput output
