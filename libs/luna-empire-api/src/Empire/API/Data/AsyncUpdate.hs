module Empire.API.Data.AsyncUpdate where

import Prologue
import Empire.API.Data.GraphLocation (GraphLocation)
import Empire.API.Data.Node          (Node)

import qualified Empire.API.Graph.NodeUpdate       as Node
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult

data AsyncUpdate = NodeUpdate   Node.Update
                 | ResultUpdate NodeResult.Update
                 deriving (Show, Eq)
