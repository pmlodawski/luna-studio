module Empire.Commands.Publisher where

import Prologue
import Empire.Empire
import Control.Monad.Reader
import Control.Monad.STM             (atomically)
import Control.Concurrent.STM.TChan  (writeTChan)
import Empire.API.Data.AsyncUpdate   (AsyncUpdate (..))
import Empire.API.Data.GraphLocation (GraphLocation)
import Empire.API.Data.Node          (Node, NodeId)
import Empire.API.Data.DefaultValue  (Value)

import qualified Empire.API.Graph.NodeUpdate       as Node
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult

notifyNodeUpdate :: GraphLocation -> Node -> Command s ()
notifyNodeUpdate loc n = do
    chan <- asks $ view updatesChan
    liftIO $ atomically $ writeTChan chan $ NodeUpdate $ Node.Update loc n

notifyResultUpdate :: GraphLocation -> NodeId -> Maybe Value -> Integer -> Command s ()
notifyResultUpdate loc nid v t = do
    chan <- asks $ view updatesChan
    liftIO $ atomically $ writeTChan chan $ ResultUpdate $ NodeResult.Update loc nid v t
