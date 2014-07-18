---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Bus.Logger.Logger where

import Control.Monad (forever)
import Data.List     (isSuffixOf)

import           Flowbox.Bus.Bus                                (Bus)
import qualified Flowbox.Bus.Bus                                as Bus
import qualified Flowbox.Bus.Data.Exception                     as Exception
import qualified Flowbox.Bus.Data.Message                       as Message
import           Flowbox.Bus.Data.MessageFrame                  (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic                         (Topic)
import qualified Flowbox.Bus.Data.Topic                         as Topic
import           Flowbox.Bus.EndPoint                           (BusEndPoints)
import           Flowbox.Prelude                                hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                   as Proto
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.Logger.Logger"


run :: BusEndPoints -> [Topic] -> IO (Either Bus.Error ())
run ep topics = Bus.runBus ep $ do logger info $ "Subscribing to topics: " ++ show topics
                                   mapM_ Bus.subscribe topics
                                   forever logMessage


logMessage :: Bus ()
logMessage = do msgFrame <- Bus.receive'
                case msgFrame of
                    Left err -> logger error $ "Unparseable message: " ++ err
                    Right (MessageFrame msg crlID senderID lastFrame) -> do
                        let topic  = msg ^. Message.topic
                            logMsg = show senderID
                                   ++ " -> "
                                   ++ show crlID
                                   ++ " (last = "
                                   ++ show lastFrame
                                   ++ ")"
                                   ++ "\t:: "
                                   ++ topic
                            content = msg ^. Message.message
                            errorMsg = case Proto.messageGet' content of
                                Left err        -> "(cannot parse error message: " ++ err ++ ")"
                                Right exception -> case (decodeP exception) ^. Exception.msg of
                                    Nothing           -> "(exception without message)"
                                    Just exceptionMsg -> exceptionMsg
                        if Topic.error `isSuffixOf` topic
                            then do logger error logMsg
                                    logger error errorMsg
                            else logger info  logMsg


