---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Interpreter.RPC.Handler.Handler where

import           Control.Monad    (forever)
import           Pipes            (lift, (>->))
import qualified Pipes
import qualified Pipes.Concurrent as Pipes

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
import qualified Flowbox.ProjectManager.RPC.Topic            as Topic
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.RPC.Handler.Handler"


handlerMap :: HandlerMap SessionT
handlerMap callback = HandlerMap.fromList
    [ (Topic.interpreterInvalidateCallRequest  , respond Topic.update $ Interpreter.invalidateCall   )
    , (Topic.interpreterInvalidateDefRequest   , respond Topic.update $ Interpreter.invalidateDef    )
    , (Topic.interpreterInvalidateNodeRequest  , respond Topic.update $ Interpreter.invalidateNode   )
    , (Topic.interpreterRunRequest             , respond Topic.update $ Interpreter.run              )
    , (Topic.interpreterWatchPointAddRequest   , respond Topic.update $ Interpreter.watchPointAdd    )
    , (Topic.interpreterWatchPointRemoveRequest, respond Topic.update $ Interpreter.watchPointRemove )
    , (Topic.interpreterWatchPointListRequest  , respond Topic.status $ Interpreter.watchPointList   )

    , (Topic.projectmanagerSyncGetRequest                           /+ status , call0 ASTWatch.projectmanagerSyncGet)

    , (Topic.projectCreateRequest                                   /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectOpenRequest                                     /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectCloseRequest                                    /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryCreateRequest                            /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryLoadRequest                              /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryUnloadRequest                            /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstRemoveRequest                         /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstModuleAddRequest                      /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstModuleModifyClsRequest                /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstModuleModifyFieldsRequest             /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstModuleModifyImportsRequest            /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstDataAddRequest                        /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionModifyInputsRequest           /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionModifyNameRequest             /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionModifyOutputRequest           /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionModifyPathRequest             /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionGraphConnectRequest           /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionGraphDisconnectRequest        /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionGraphNodeAddRequest           /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionGraphNodeRemoveRequest        /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyRequest        /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionGraphNodeModifyinplaceRequest /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultRemoveRequest /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    , (Topic.projectLibraryAstFunctionGraphNodeDefaultSetRequest    /+ update , query Topic.projectmanagerSyncGetRequest ASTWatch.test3)
    ]
    where
        respond type_ = callback ((/+) type_) . Processor.singleResult

        query topic = callback (const topic) . Processor.singleResult

        call0 :: Proto.Serializable a => (a -> RPC SessionT ()) -> SessionT [Message]
        call0 = callback id . Processor.noResult


interpret :: Pipes.Pipe (Message, Message.CorrelationID)
                        (Message, Message.CorrelationID)
                        SessionT ()
interpret = forever $ do
    (message, crl) <- Pipes.await
    results <- lift $ Processor.processLifted handlerMap message
    mapM_ (\r -> Pipes.yield (r, crl)) results



run :: (Pipes.Input  (Message, Message.CorrelationID),
        Pipes.Output (Message, Message.CorrelationID))
    -> IO (Either Error ())
run (input, output) = Session.run def $ SessionT.runSessionT $ do
    Pipes.runEffect $ Pipes.fromInput input
                  >-> interpret
                  >-> Pipes.toOutput output
