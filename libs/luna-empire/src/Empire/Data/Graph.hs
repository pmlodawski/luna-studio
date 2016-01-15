module Empire.Data.Graph where

import           Prologue
import qualified Empire.Utils.IdGen     as IdGen
import           Empire.Data.AST        (AST)
import qualified Data.IntMap            as IntMap
import           Data.IntMap            (IntMap)
import           Luna.Syntax.Repr.Graph (Ref, Node)
import           Empire.API.Data.Node   (NodeId)

data Graph = Graph { _ast         :: AST
                   , _nodeMapping :: IntMap (Ref Node)
                   } deriving (Show)

makeLenses ''Graph

instance Default Graph where
    def = Graph def def

nextNodeId :: Graph -> NodeId
nextNodeId graph = IdGen.nextId $ graph ^. nodeMapping
