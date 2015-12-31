module BatchConnector.Monadic.Commands where

import           Utils.PreludePlus
import           Empire.API.Data.Node    (Node)
import           Batch.Workspace         (Workspace)
import qualified BatchConnector.Commands as BatchCmd
import           Control.Monad.State
import           Data.Text.Lazy          (Text)

import           Reactive.Commands.Command (Command, ioCommand, performIO)

runMain :: Command Workspace ()
runMain = performIO BatchCmd.runMain

getCode :: Command Workspace ()
getCode = ioCommand BatchCmd.getCode

setCode :: Text -> Command Workspace ()
setCode = ioCommand . BatchCmd.setCode

insertSerializationMode :: Node -> Command Workspace ()
insertSerializationMode = ioCommand . BatchCmd.insertSerializationMode

insertSerializationModes :: [Node] -> Command Workspace ()
insertSerializationModes = ioCommand . BatchCmd.insertSerializationModes

updateNode :: Node -> Command Workspace ()
updateNode = ioCommand . BatchCmd.updateNode

requestValues :: [Node] -> Command Workspace ()
requestValues = ioCommand . BatchCmd.requestValues
