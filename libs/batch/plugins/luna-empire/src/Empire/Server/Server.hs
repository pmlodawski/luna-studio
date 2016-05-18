{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Empire.Server.Server where

import           Control.Concurrent.STM.TChan (writeTChan)
import           Control.Monad.State          (StateT)
import           Control.Monad.STM            (atomically)
import           Data.Binary                  (Binary)
import qualified Data.Binary                  as Bin
import           Data.ByteString.Lazy         (toStrict)
import           Prologue

import qualified Empire.API.Response          as Response
import           Empire.API.Topic             (MessageTopic)
import qualified Empire.API.Topic             as Topic
import           Empire.Env                   (Env)
import qualified Empire.Env                   as Env
import qualified Flowbox.Bus.Bus              as Bus
import           Flowbox.Bus.BusT             (BusT (..))
import qualified Flowbox.Bus.Data.Message     as Message
import qualified Flowbox.System.Log.Logger    as Logger

sendToBus :: Binary a => String -> a -> StateT Env BusT ()
sendToBus topic bin = do
    chan <- use Env.toBusChan
    liftIO $ atomically $ writeTChan chan $ Message.Message topic $ toStrict $ Bin.encode bin

sendToBus' :: (MessageTopic a, Binary a) => a -> StateT Env BusT ()
sendToBus' msg = sendToBus (Topic.topic msg) msg

replyFail :: forall a b. (Binary a, Response.ResponseResult a b) => Logger.LoggerIO -> String -> a -> StateT Env BusT ()
replyFail logger errMsg req = do
  logger Logger.error $ formatErrorMessage req errMsg
  sendToBus' $ Response.error req errMsg

replyOk :: forall a b. (Binary a, Response.ResponseResult a (), MessageTopic (Response.Response a ())) => a -> StateT Env BusT ()
replyOk req = sendToBus' $ Response.ok req

replyResult :: forall a b. (Binary a, Binary b, Response.ResponseResult a b) => a -> b -> StateT Env BusT ()
replyResult req res = sendToBus' $ Response.result req res

errorMessage :: String
errorMessage = "Error processing request: "

formatErrorMessage :: MessageTopic a => a -> String -> String
formatErrorMessage req msg = errorMessage <> (Topic.topic req) <> ": " <> msg
