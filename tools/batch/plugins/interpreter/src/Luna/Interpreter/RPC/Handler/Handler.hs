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
import           Pipes                     ((>->))
import qualified Pipes
import qualified Pipes.Concurrent          as Pipes

import           Flowbox.Bus.Data.Message                 (Message)
import qualified Flowbox.Bus.Data.Message                 as Message
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
import qualified Luna.Interpreter.RPC.Handler.Value       as Value
import qualified Luna.Interpreter.RPC.Topic               as Topic
import qualified Luna.Interpreter.Session.Env             as Env
import           Luna.Interpreter.Session.Error           (Error)
import qualified Luna.Interpreter.Session.Session         as Session
import           Luna.Interpreter.Session.SessionT        (SessionT)
import qualified Luna.Interpreter.Session.SessionT        as SessionT



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.RPC.Handler.Handler"


handlerMap :: HandlerMap Context SessionT
handlerMap callback = HandlerMap.fromList
    [ (Topic.interpreterRunRequest             , respond Topic.update Interpreter.run              )
    , (Topic.interpreterWatchPointAddRequest   , respond Topic.update Interpreter.watchPointAdd    )
    , (Topic.interpreterWatchPointRemoveRequest, respond Topic.update Interpreter.watchPointRemove )
    , (Topic.interpreterWatchPointListRequest  , respond Topic.status Interpreter.watchPointList   )
    , (Topic.interpreterValueRequest           , respond Topic.update Value.get                    )

    , (Topic.projectmanagerSyncGetRequest                           /+ status, call0 ASTWatch.projectmanagerSyncGet)

    , (Topic.projectCreateRequest                                   /+ update, sync ASTWatch.projectCreate)
    , (Topic.projectOpenRequest                                     /+ update, sync ASTWatch.projectOpen)
    , (Topic.projectCloseRequest                                    /+ update, sync ASTWatch.projectClose)
    , (Topic.projectLibraryCreateRequest                            /+ update, sync ASTWatch.libraryCreate)
    , (Topic.projectLibraryLoadRequest                              /+ update, sync ASTWatch.libraryLoad)
    , (Topic.projectLibraryUnloadRequest                            /+ update, sync ASTWatch.libraryUnload)
    , (Topic.projectLibraryAstRemoveRequest                         /+ update, sync ASTWatch.astRemove)
    , (Topic.projectLibraryAstModuleAddRequest                      /+ update, sync ASTWatch.astModuleAdd)
    , (Topic.projectLibraryAstModuleModifyClsRequest                /+ update, sync ASTWatch.astModuleModifyCls)
    , (Topic.projectLibraryAstModuleModifyFieldsRequest             /+ update, sync ASTWatch.astModuleModifyFields)
    , (Topic.projectLibraryAstModuleModifyImportsRequest            /+ update, sync ASTWatch.astModuleModifyImports)
    , (Topic.projectLibraryAstDataAddRequest                        /+ update, sync ASTWatch.astDataAdd)
    , (Topic.projectLibraryAstDataModifyClassesRequest              /+ update, sync ASTWatch.astDataModifyClasses)
    , (Topic.projectLibraryAstDataModifyClsRequest                  /+ update, sync ASTWatch.astDataModifyCls)
    , (Topic.projectLibraryAstDataModifyConsRequest                 /+ update, sync ASTWatch.astDataModifyCons)
    , (Topic.projectLibraryAstDataModifyMethodsRequest              /+ update, sync ASTWatch.astDataModifyMethods)
    , (Topic.projectLibraryAstFunctionAddRequest                    /+ update, sync ASTWatch.astFunctionAdd)
    , (Topic.projectLibraryAstFunctionModifyInputsRequest           /+ update, sync ASTWatch.astFunctionModifyInputs)
    , (Topic.projectLibraryAstFunctionModifyNameRequest             /+ update, sync ASTWatch.astFunctionModifyName)
    , (Topic.projectLibraryAstFunctionModifyOutputRequest           /+ update, sync ASTWatch.astFunctionModifyOutput)
    , (Topic.projectLibraryAstFunctionModifyPathRequest             /+ update, sync ASTWatch.astFunctionModifyPath)
    , (Topic.projectLibraryAstFunctionGraphConnectRequest           /+ update, sync ASTWatch.graphConnect)
    , (Topic.projectLibraryAstFunctionGraphDisconnectRequest        /+ update, sync ASTWatch.graphDisconnect)
    , (Topic.projectLibraryAstFunctionGraphNodeAddRequest           /+ update, sync ASTWatch.graphNodeAdd)
    , (Topic.projectLibraryAstFunctionGraphNodeRemoveRequest        /+ update, sync ASTWatch.graphNodeRemove)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyRequest        /+ update, sync ASTWatch.graphNodeModify)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest /+ update, sync ASTWatch.graphNodeModifyInPlace)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultRemoveRequest /+ update, sync ASTWatch.graphNodeDefaultRemove)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest    /+ update, sync ASTWatch.graphNodeDefaultSet)
    ]
    where
        respond type_ = callback (/+ type_) . Processor.singleResult

        --query topic = callback (const topic) . Processor.singleResult

        call0 :: Proto.Serializable a => (a -> RPC Context SessionT ()) -> StateT Context SessionT [Message]
        call0 = callback id . Processor.noResult

        sync :: Proto.Serializable a => (a -> RPC Context SessionT ()) -> StateT Context SessionT [Message]
        sync = callback (const Topic.projectmanagerSyncGetRequest) . Processor.optResult . (.) ASTWatch.syncIfNeeded


interpret :: Pipes.Pipe (Message, Message.CorrelationID)
                        (Message, Maybe Message.CorrelationID)
                        (StateT Context SessionT) ()
interpret = forever $ do
    (message, crl) <- Pipes.await
    results <- lift $ Processor.processLifted handlerMap message
    mapM_ (\r -> Pipes.yield (r, Just crl)) results


run :: Config -> Context
    -> (Pipes.Input  (Message, Message.CorrelationID),
        Pipes.Output (Message, Maybe Message.CorrelationID))
    -> IO (Either Error ())
run cfg ctx (input, output) =
    Session.run cfg env $ SessionT.runSessionT $ flip evalStateT ctx $
        Pipes.runEffect $ Pipes.fromInput input
                      >-> interpret
                      >-> Pipes.toOutput output
    where
        env = def & Env.resultCallBack .~ Value.reportOutputValue output



