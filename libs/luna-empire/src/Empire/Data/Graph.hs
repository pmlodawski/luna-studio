module Empire.Data.Graph where

import           Prologue
import           Empire.Data.AST        (AST)
import qualified Data.IntMap            as IntMap
import           Data.IntMap            (IntMap)
import           Luna.Syntax.Repr.Graph (Ref, Node)
import           Empire.Objects.Node    (NodeId)

data Graph = Graph { _ast         :: AST
                   , _nodeMapping :: IntMap (Ref Node)
                   } deriving (Show)

makeLenses ''Graph

instance Default Graph where
    def = Graph def def

nextNodeId :: Graph -> NodeId
nextNodeId graph = if IntMap.null nodesMap then 0 else 1 + (fst . IntMap.findMax $ nodesMap) where
    nodesMap = graph ^. nodeMapping
