module Empire.Commands.Publisher where

import           Control.Concurrent.STM.TChan      (writeTChan)
import           Control.Monad.Reader
import           Control.Monad.STM                 (atomically)
import           Empire.API.Data.AsyncUpdate       (AsyncUpdate (..))
import           Empire.API.Data.Connection        (Connection)
import           Empire.API.Data.GraphLocation     (GraphLocation)
import           Empire.API.Data.Node              (Node, NodeId)
import           Empire.API.Data.PortRef           (InPortRef, OutPortRef)
import           Empire.Data.Graph                 (Graph)
import           Empire.Empire
import           Prologue

import qualified Empire.API.Data.Connection        as Connection
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.NodesUpdate      as Node
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult

notifyNodesUpdate :: GraphLocation -> Node -> Command s ()
notifyNodesUpdate loc n = do
    chan <- asks $ view updatesChan
    liftIO $ atomically $ writeTChan chan $ NodesUpdate $ Node.Update loc [n]

notifyConnectionUpdate :: GraphLocation -> OutPortRef -> InPortRef -> Command s ()
notifyConnectionUpdate loc outPort inPort = do
    chan <- ask $ view updatesChan
    liftIO $ atomically $ writeTChan chan $ ConnectionUpdate $ Connect.Update loc outPort inPort

notifyResultUpdate :: GraphLocation -> NodeId -> NodeResult.NodeValue -> Integer -> Command s ()
notifyResultUpdate loc nid v t = do
    chan <- asks $ view updatesChan
    liftIO $ atomically $ writeTChan chan $ ResultUpdate $ NodeResult.Update loc nid v t

requestTC :: GraphLocation -> Graph -> Bool -> Command s ()
requestTC loc g flush = do
    chan <- asks $ view typecheckChan
    liftIO $ atomically $ writeTChan chan (loc, g, flush)
