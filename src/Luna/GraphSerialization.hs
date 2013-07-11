module Luna.GraphSerialization where

import Control.Monad
import qualified  Data.Graph.Inductive as DG
import Data.Serialize
import Data.Word

import qualified Luna.Edge as Edge
import qualified Luna.Node as Node
import qualified Luna.NodeDef as NodeDef

instance Serialize Node.Node where
  put i = case i of 
            Node.TypeNode     name     -> put ((0 :: Word8), name)
            Node.CallNode     name     -> put ((1 :: Word8), name)
            Node.ClassNode    name def -> put ((2 :: Word8), name, def)
            Node.FunctionNode name def -> put ((3 :: Word8), name, def)
            Node.PackageNode  name def -> put ((4 :: Word8), name, def)

  get   = do 
            t <- get :: Get Word8
            case t of 
              0 -> do name        <- get; return $ Node.TypeNode name
              1 -> do name        <- get; return $ Node.CallNode name
              2 -> do (name, def) <- get; return $ Node.ClassNode    name def
              3 -> do (name, def) <- get; return $ Node.FunctionNode name def
              4 -> do (name, def) <- get; return $ Node.PackageNode  name def


instance Serialize NodeDef.NodeDef where
  put i = put (NodeDef.inputs i, NodeDef.outputs i, NodeDef.imports i, NodeDef.graph i, NodeDef.libID i)
  get   = do
            (inputs, outputs, imports, graph, libID) <- get
            return $ NodeDef.NodeDef inputs outputs imports graph libID

instance Serialize Edge.EdgeCls where
  put i = put $ show i
  get   = liftM (read :: String -> Edge.EdgeCls) (get :: Get String)

instance Serialize Edge.Edge where
  put i = put (Edge.inn i, Edge.out i, Edge.cls i)
  get   = do
            (x,y,z) <- get
            return $ Edge.Edge x y z

instance (Serialize a, Serialize b) => Serialize (DG.Gr a b) where
  put i = put (DG.labNodes i, DG.labEdges i)
  get = do
          (nd, edg) <- get
          return $ DG.mkGraph nd edg
