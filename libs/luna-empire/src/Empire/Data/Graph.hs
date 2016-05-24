module Empire.Data.Graph where

import           Prologue
import qualified Empire.Utils.IdGen     as IdGen
import           Empire.Data.AST        (AST, NodeRef)
import           Data.Map.Lazy          (Map)
import qualified Data.Map.Lazy          as Map
import           Data.IntMap            (IntMap)
import           Empire.API.Data.Node   (NodeId)

import           Luna.Syntax.Model.Network.Builder (star, runNetworkBuilderT)

data Graph = Graph { _ast         :: AST
                   , _nodeMapping :: Map NodeId NodeRef
                   } deriving (Show)

makeLenses ''Graph

instance Default Graph where
    def = Graph defaultAST def

-- nextNodeId :: Graph -> Int
-- nextNodeId graph = IdGen.nextId $ graph ^. nodeMapping

defaultAST :: AST
defaultAST = snd $ (runIdentity $ runNetworkBuilderT def $ star :: (NodeRef, AST))
