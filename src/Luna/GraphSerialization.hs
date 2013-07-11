module Luna.GraphSerialization where

import Control.Monad
import qualified  Data.Graph.Inductive as DG
import Data.Serialize
import Data.Word

import qualified Luna.Graph as Graph
import qualified Luna.Edge as Edge
import qualified Luna.NodeDef as NodeDef

instance Serialize Graph.Node where
  put i = case i of 
            Graph.TypeNode  name -> put ((0 :: Word8), name)
            Graph.CallNode  name -> put ((1 :: Word8), name)
            Graph.ClassNode    name graph def -> put ((2 :: Word8), name, graph, def)
            Graph.FunctionNode name graph def -> put ((3 :: Word8), name, graph, def)
            Graph.PackageNode  name graph def -> put ((4 :: Word8), name, graph, def)

  get   = do 
            t <- get :: Get Word8
            case t of 
              0 -> do name <- get; return $ Graph.TypeNode name
              1 -> do name <- get; return $ Graph.CallNode name
              2 -> do (name, graph, def) <- get; return $ Graph.ClassNode    name graph def
              3 -> do (name, graph, def) <- get; return $ Graph.FunctionNode name graph def
              4 -> do (name, graph, def) <- get; return $ Graph.PackageNode  name graph def

instance Serialize NodeDef.NodeDef where
  put i = put (NodeDef.inputs i, NodeDef.outputs i, NodeDef.imports i)
  get   = do
            (inputs, outputs, imports) <- get
            return $ NodeDef.NodeDef inputs outputs imports

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
