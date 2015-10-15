module BatchConnector.Monadic.Commands where

import           Utils.PreludePlus
import           Object.Node             (Node)
import           Batch.Workspace         (Workspace)
import qualified BatchConnector.Commands as BatchCmd
import           Control.Monad.State

import           Reactive.Plugins.Core.Action.Commands.Command (Command, ioCommand, performIO)

runMain :: Command Workspace ()
runMain = performIO BatchCmd.runMain

getCode :: Command Workspace ()
getCode = ioCommand BatchCmd.getCode

insertSerializationMode :: Node -> Command Workspace ()
insertSerializationMode = ioCommand . BatchCmd.insertSerializationMode

insertSerializationModes :: [Node] -> Command Workspace ()
insertSerializationModes = ioCommand . BatchCmd.insertSerializationModes

requestValues :: [Node] -> Command Workspace ()
requestValues = ioCommand . BatchCmd.requestValues
