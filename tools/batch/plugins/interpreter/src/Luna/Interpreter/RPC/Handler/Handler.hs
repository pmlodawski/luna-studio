---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Luna.Interpreter.RPC.Handler.Handler where

import           Control.Monad             (forever)
import           Control.Monad.Trans.State
import           Data.IORef                (IORef)
import qualified Data.IORef                as IORef
import           Pipes                     (liftIO, (>->))
import qualified Pipes
import qualified Pipes.Concurrent          as Pipes
import qualified Pipes.Safe                as Pipes

import           Flowbox.Bus.Data.Flag                    (Flag)
import qualified Flowbox.Bus.Data.Flag                    as Flag
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
import qualified Flowbox.Control.Concurrent               as Concurrent
import           Flowbox.Prelude                          hiding (Context, error)
import           Flowbox.ProjectManager.Context           (Context)
import qualified Flowbox.ProjectManager.RPC.Topic         as Topic
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers             as Proto
import qualified Luna.Interpreter.RPC.Handler.Abort       as Abort
import qualified Luna.Interpreter.RPC.Handler.ASTWatch    as ASTWatch
import qualified Luna.Interpreter.RPC.Handler.Interpreter as Interpreter
import qualified Luna.Interpreter.RPC.Handler.Preprocess  as Preprocess
import qualified Luna.Interpreter.RPC.Handler.Sync        as Sync
import qualified Luna.Interpreter.RPC.Handler.Value       as Value
import qualified Luna.Interpreter.RPC.Topic               as Topic
import qualified Luna.Interpreter.Session.Env             as Env
import           Luna.Interpreter.Session.Error           (Error)
import           Luna.Interpreter.Session.Session         (SessionST)
import qualified Luna.Interpreter.Session.Session         as Session



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


handlerMap :: Prefix -> HandlerMap Context SessionST
handlerMap prefix callback = HandlerMap.fromList $ Prefix.prefixifyTopics prefix
    [ (Topic.interpreterSetProjectIDRequest                , respond Topic.update Interpreter.setProjectID    )
    , (Topic.interpreterGetProjectIDRequest                , respond Topic.status Interpreter.getProjectID    )
    , (Topic.interpreterSetMainPtrRequest                  , respond Topic.update Interpreter.setMainPtr      )
    , (Topic.interpreterGetMainPtrRequest                  , respond Topic.status Interpreter.getMainPtr      )
    , (Topic.interpreterRunRequest                         , respond Topic.update Interpreter.run             )
    , (Topic.interpreterWatchPointAddRequest               , respond Topic.update Interpreter.watchPointAdd   )
    , (Topic.interpreterWatchPointRemoveRequest            , respond Topic.update Interpreter.watchPointRemove)
    , (Topic.interpreterWatchPointListRequest              , respond Topic.status Interpreter.watchPointList  )
    , (Topic.interpreterValueRequest                       , respond Topic.update Value.get                   )
    , (Topic.interpreterPingRequest                        , respond Topic.status Interpreter.ping            )
    , (Topic.interpreterAbortRequest                       , respond Topic.status Interpreter.abort           )
    , (Topic.interpreterSerializationModeDefaultGetRequest , respond Topic.status Interpreter.getDefaultSerializationMode)
    , (Topic.interpreterSerializationModeDefaultSetRequest , respond Topic.update Interpreter.setDefaultSerializationMode)
    , (Topic.interpreterSerializationModeGetRequest        , respond Topic.status Interpreter.getSerializationMode       )
    , (Topic.interpreterSerializationModeInsertRequest     , respond Topic.update Interpreter.insertSerializationMode    )
    , (Topic.interpreterSerializationModeDeleteRequest     , respond Topic.update Interpreter.deleteSerializationMode    )
    , (Topic.interpreterSerializationModeDeleteAllRequest  , respond Topic.update Interpreter.deleteAllSerializationMode )
    , (Topic.interpreterMemoryGetLimitsRequest             , respond Topic.status Interpreter.getMemoryLimits)
    , (Topic.interpreterMemorySetLimitsRequest             , respond Topic.update Interpreter.setMemoryLimits)

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
extraImports = ["FlowboxM.Libs.Flowbox.Std"]


interpret :: Prefix
          -> IORef Message.CorrelationID
          -> Pipes.Pipe (Message, Message.CorrelationID)
                        (Message, Message.CorrelationID, Flag)
                        (Pipes.SafeT (StateT Context SessionST)) ()
interpret prefix crlRef = forever $ do
    (message, crl) <- Pipes.await
    liftIO $ IORef.writeIORef crlRef crl
    results <- lift2 $ Processor.process (handlerMap prefix) message
    let send []    = return ()
        send [r]   = Pipes.yield (r, crl, Flag.Enable)
        send (r:t) = Pipes.yield (r, crl, Flag.Disable) >> send t
    send results


run :: Config -> Prefix -> Context
    -> (Pipes.Input  (Message, Message.CorrelationID),
        Pipes.Output (Message, Message.CorrelationID, Flag))
    -> IO (Either Error ())
run cfg prefix ctx (input, output) = do
    crlRef              <- IORef.newIORef def
    interpreterThreadId <- Concurrent.myThreadId
    (output1, input1)   <- Pipes.spawn Pipes.Unbounded
    let env = def & Env.resultCallBack .~ Value.reportOutputValue crlRef output
    Concurrent.forkIO_ $ Pipes.runEffect $
            Pipes.fromInput input
        >-> Preprocess.preprocess prefix interpreterThreadId
        >-> Pipes.toOutput output1
    Session.run cfg env extraImports $ lift $ flip evalStateT ctx $ forever $
         Pipes.runSafeT $ Pipes.runEffect $ Abort.handleAbort $
                Pipes.fromInput input1
            >-> interpret prefix crlRef
            >-> Pipes.toOutput output

