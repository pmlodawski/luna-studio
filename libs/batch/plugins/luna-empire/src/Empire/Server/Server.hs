{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
module Empire.Server.Server where

import           Control.Concurrent.STM.TChan (writeTChan)
import           Control.Monad.State          (StateT)
import           Control.Monad.STM            (atomically)
import           Data.Binary                  (Binary)
import qualified Data.Binary                  as Bin
import           Data.ByteString.Lazy         (toStrict)
import           Prologue

import           Empire.API.Request           (Request)
import qualified Empire.API.Response          as Response
import           Empire.API.Topic             (MessageTopic)
import qualified Empire.API.Topic             as Topic
import           Empire.Env                   (Env)
import qualified Empire.Env                   as Env
import qualified System.Log.MLogger           as Logger
import qualified ZMQ.Bus.Bus                  as Bus
import qualified ZMQ.Bus.Data.Message         as Message
import           ZMQ.Bus.Trans                (BusT (..))

sendToBus :: Binary a => String -> a -> StateT Env BusT ()
sendToBus topic bin = do
    chan <- use Env.toBusChan
    liftIO $ atomically $ writeTChan chan $ Message.Message topic $ toStrict $ Bin.encode bin

sendToBus' :: (MessageTopic a, Binary a) => a -> StateT Env BusT ()
sendToBus' msg = sendToBus (Topic.topic msg) msg

replyFail :: forall a b. (Binary a, Response.ResponseResult a b) => Logger.Logger -> String -> Request a -> StateT Env BusT ()
replyFail logger errMsg req = do
  logger Logger.error $ formatErrorMessage req errMsg
  sendToBus' $ Response.error req errMsg

replyOk :: forall a b. (Binary a, Response.ResponseResult a (), MessageTopic (Response.Response a ())) => Request a -> StateT Env BusT ()
replyOk req = sendToBus' $ Response.ok req

replyResult :: forall a b. (Binary a, Binary b, Response.ResponseResult a b) => Request a -> b -> StateT Env BusT ()
replyResult req res = sendToBus' $ Response.result req res

errorMessage :: String
errorMessage = "Error processing request: "

formatErrorMessage :: MessageTopic a => a -> String -> String
formatErrorMessage req msg = errorMessage <> (Topic.topic req) <> ": " <> msg
