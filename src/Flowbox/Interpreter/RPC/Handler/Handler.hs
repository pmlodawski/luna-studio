---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Interpreter.RPC.Handler.Handler where

import           Control.Monad             (forever)
import           Control.Monad.Trans.State
import           Pipes                     ((>->))
import qualified Pipes
import qualified Pipes.Concurrent          as Pipes

import           Flowbox.Bus.Data.Message                    (Message)
import qualified Flowbox.Bus.Data.Message                    as Message
import           Flowbox.Bus.Data.Topic                      (status, update, (/+))
import qualified Flowbox.Bus.Data.Topic                      as Topic
import           Flowbox.Bus.RPC.HandlerMap                  (HandlerMap)
import qualified Flowbox.Bus.RPC.HandlerMap                  as HandlerMap
import           Flowbox.Bus.RPC.RPC                         (RPC)
import qualified Flowbox.Bus.RPC.Server.Processor            as Processor
import qualified Flowbox.Interpreter.RPC.Handler.ASTWatch    as ASTWatch
import qualified Flowbox.Interpreter.RPC.Handler.Interpreter as Interpreter
import qualified Flowbox.Interpreter.RPC.Topic               as Topic
import           Flowbox.Interpreter.Session.Error           (Error)
import qualified Flowbox.Interpreter.Session.Session         as Session
import           Flowbox.Interpreter.Session.SessionT        (SessionT)
import qualified Flowbox.Interpreter.Session.SessionT        as SessionT
import           Flowbox.Prelude                             hiding (Context, error)
import           Flowbox.ProjectManager.Context              (Context)
import qualified Flowbox.ProjectManager.RPC.Topic            as Topic
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.RPC.Handler.Handler"


handlerMap :: HandlerMap Context SessionT
handlerMap callback = HandlerMap.fromList
    [ (Topic.interpreterInvalidateCallRequest  , respond Topic.update Interpreter.invalidateCall   )
    , (Topic.interpreterInvalidateDefRequest   , respond Topic.update Interpreter.invalidateDef    )
    , (Topic.interpreterInvalidateNodeRequest  , respond Topic.update Interpreter.invalidateNode   )
    , (Topic.interpreterRunRequest             , respond Topic.update Interpreter.run              )
    , (Topic.interpreterWatchPointAddRequest   , respond Topic.update Interpreter.watchPointAdd    )
    , (Topic.interpreterWatchPointRemoveRequest, respond Topic.update Interpreter.watchPointRemove )
    , (Topic.interpreterWatchPointListRequest  , respond Topic.status Interpreter.watchPointList   )

    , (Topic.projectmanagerSyncGetRequest                           /+ status, call0 ASTWatch.projectmanagerSyncGet)

    , (Topic.projectCreateRequest                                   /+ update, call0 ASTWatch.projectCreate)
    , (Topic.projectOpenRequest                                     /+ update, call0 ASTWatch.projectOpen)
    , (Topic.projectCloseRequest                                    /+ update, call0 ASTWatch.projectClose)
    , (Topic.projectLibraryCreateRequest                            /+ update, call0 ASTWatch.libraryCreate)
    , (Topic.projectLibraryLoadRequest                              /+ update, call0 ASTWatch.libraryLoad)
    , (Topic.projectLibraryUnloadRequest                            /+ update, call0 ASTWatch.libraryUnload)
    , (Topic.projectLibraryAstRemoveRequest                         /+ update, call0 ASTWatch.astRemove)
    , (Topic.projectLibraryAstModuleAddRequest                      /+ update, call0 ASTWatch.astModuleAdd)
    , (Topic.projectLibraryAstModuleModifyClsRequest                /+ update, call0 ASTWatch.astModuleModifyCls)
    , (Topic.projectLibraryAstModuleModifyFieldsRequest             /+ update, call0 ASTWatch.astModuleModifyFields)
    , (Topic.projectLibraryAstModuleModifyImportsRequest            /+ update, call0 ASTWatch.astModuleModifyImports)
    , (Topic.projectLibraryAstDataAddRequest                        /+ update, call0 ASTWatch.astDataAdd)
    , (Topic.projectLibraryAstDataModifyClassesRequest              /+ update, call0 ASTWatch.astDataModifyClasses)
    , (Topic.projectLibraryAstDataModifyClsRequest                  /+ update, call0 ASTWatch.astDataModifyCls)
    , (Topic.projectLibraryAstDataModifyConsRequest                 /+ update, call0 ASTWatch.astDataModifyCons)
    , (Topic.projectLibraryAstDataModifyMethodsRequest              /+ update, call0 ASTWatch.astDataModifyMethods)
    , (Topic.projectLibraryAstFunctionAddRequest                    /+ update, call0 ASTWatch.astFunctionAdd)
    , (Topic.projectLibraryAstFunctionModifyInputsRequest           /+ update, call0 ASTWatch.astFunctionModifyInputs)
    , (Topic.projectLibraryAstFunctionModifyNameRequest             /+ update, call0 ASTWatch.astFunctionModifyName)
    , (Topic.projectLibraryAstFunctionModifyOutputRequest           /+ update, call0 ASTWatch.astFunctionModifyOutput)
    , (Topic.projectLibraryAstFunctionModifyPathRequest             /+ update, call0 ASTWatch.astFunctionModifyPath)
    , (Topic.projectLibraryAstFunctionGraphConnectRequest           /+ update, call0 ASTWatch.graphConnect)
    , (Topic.projectLibraryAstFunctionGraphDisconnectRequest        /+ update, call0 ASTWatch.graphDisconnect)
    , (Topic.projectLibraryAstFunctionGraphNodeAddRequest           /+ update, call0 ASTWatch.graphNodeAdd)
    , (Topic.projectLibraryAstFunctionGraphNodeRemoveRequest        /+ update, call0 ASTWatch.graphNodeRemove)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyRequest        /+ update, call0 ASTWatch.graphNodeModify)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest /+ update, call0 ASTWatch.graphNodeModifyInPlace)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultRemoveRequest /+ update, call0 ASTWatch.graphNodeDefaultRemove)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest    /+ update, call0 ASTWatch.graphNodeDefaultSet)
    ]
    where
        respond type_ = callback (/+ type_) . Processor.singleResult

        query topic = callback (const topic) . Processor.singleResult

        call0 :: Proto.Serializable a => (a -> RPC Context SessionT ()) -> StateT Context SessionT [Message]
        call0 = callback id . Processor.noResult


interpret :: Pipes.Pipe (Message, Message.CorrelationID)
                        (Message, Message.CorrelationID)
                        (StateT Context SessionT) ()
interpret = forever $ do
    (message, crl) <- Pipes.await
    results <- lift $ Processor.processLifted handlerMap message
    mapM_ (\r -> Pipes.yield (r, crl)) results


run :: Context -> (Pipes.Input  (Message, Message.CorrelationID),
                   Pipes.Output (Message, Message.CorrelationID))
    -> IO (Either Error ())
run ctx (input, output) = Session.run def $ SessionT.runSessionT $ flip evalStateT ctx $
        Pipes.runEffect $ Pipes.fromInput input
                      >-> interpret
                      >-> Pipes.toOutput output
