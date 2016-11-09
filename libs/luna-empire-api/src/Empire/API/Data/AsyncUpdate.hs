module Empire.API.Data.AsyncUpdate where

import           Empire.API.Data.GraphLocation     (GraphLocation)
import           Empire.API.Data.Node              (Node)
import           Prologue

import qualified Empire.API.Graph.Node             as Node
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult

data AsyncUpdate = NodeUpdate   Node.Update
                 | ResultUpdate NodeResult.Update
                 deriving (Show, Eq)
