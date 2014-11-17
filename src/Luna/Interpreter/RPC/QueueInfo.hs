---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.RPC.QueueInfo where

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar

import qualified Flowbox.Bus.Data.Message           as Message
import qualified Flowbox.Control.Concurrent         as Concurrent
import           Flowbox.Prelude
import qualified Luna.Interpreter.RPC.Handler.Abort as Abort
import qualified Luna.Interpreter.Session.Env       as Env



type QueueInfo = MVar QueueInfoData


data QueueInfoData = QueueInfoData { _skipUntil   :: Maybe Message.CorrelationID
                                   , _isExecuting :: Bool
                                   } deriving (Show)

makeLenses ''QueueInfoData


instance Default QueueInfoData where
    def = QueueInfoData Nothing False


mk :: IO QueueInfo
mk = MVar.newMVar def


enterRun :: QueueInfo -> Message.CorrelationID -> IO ()
enterRun queueInfo crl = MVar.modifyMVarMasked_ queueInfo $ \qidata ->
    case qidata ^. skipUntil of
        Just targetCrl -> if targetCrl == crl
                            then return $ qidata & skipUntil   .~ Nothing
                                                 & isExecuting .~ True
                            else fail "Skipping run operation"
        Nothing -> return $ qidata & isExecuting .~ True


quitRun :: QueueInfo -> IO ()
quitRun queueInfo = MVar.modifyMVarMasked_ queueInfo $ \qidata ->
    return $ qidata & isExecuting .~ False


overrideRun :: QueueInfo -> Message.CorrelationID -> Env.FragileMVar -> Concurrent.ThreadId -> IO ()
overrideRun queueInfo crl fm threadId = MVar.modifyMVarMasked_ queueInfo $ \qidata ->
    if qidata ^. isExecuting
        then Abort.abort fm threadId >>  return (qidata & isExecuting .~ False)
        else return (qidata & skipUntil .~ Just crl)
