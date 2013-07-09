module Luna.GraphSerialization where

import Control.Monad

import qualified  Data.Graph.Inductive as DG

import qualified Luna.Graph
import qualified Luna.Node
import qualified Luna.Edge
import Data.Serialize


instance Serialize Luna.Node.Node where
  put i = put $ Luna.Node.defName i
  get   = liftM Luna.Node.Node (get :: Get String)

instance Serialize Luna.Edge.EdgeCls where
  put i = put $ show i
  get   = liftM (read :: String -> Luna.Edge.EdgeCls) (get :: Get String)

instance Serialize Luna.Edge.Edge where
  put i = put (Luna.Edge.inn i, Luna.Edge.out i, Luna.Edge.cls i)
  get   = do
            (x,y,z) <- get
            return $ Luna.Edge.Edge x y z

instance (Serialize a, Serialize b) => Serialize (DG.Gr a b) where
  put i = put (DG.labNodes i, DG.labEdges i)
  get = do
          (nd, edg) <- get
          return $ DG.mkGraph nd edg
