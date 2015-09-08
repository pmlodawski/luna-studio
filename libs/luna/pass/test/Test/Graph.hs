{-# LANGUAGE MultiParamTypeClasses #-}

import           Control.Lens
import           Data.Default
import           Data.IntMap        (IntMap)
import qualified Data.IntMap        as IntMap
import           Data.IntSet        (IntSet)
import qualified Data.IntSet        as IntSet
import           Data.Monoid
import           Prelude            hiding (id)
import           System.Environment (getArgs)


class Element n where
    id :: n -> Int

class Binding b where
    start :: b -> Int
    end   :: b -> Int

data NodeCtx a = NodeCtx IntSet IntSet a
               deriving (Show)

nodeCtx = NodeCtx mempty mempty

data EdgeCtx a = EdgeCtx Int Int a
               deriving (Show)


class GraphClass g where
    empty    :: g n e -> Bool
    insNode  :: Element n              => n  -> g n e -> g n e
    insEdge  :: (Element e, Binding e) => e  -> g n e -> g n e
    delNode  :: Element nx             => nx -> g n e -> g n e
    delEdge  :: Element nx             => nx -> g n e -> g n e



data Graph n e = Graph (IntMap (NodeCtx n))
                       (IntMap (EdgeCtx e))
               deriving (Show)


instance GraphClass Graph where
    empty (Graph ns _) = IntMap.null ns
    insNode n (Graph ns es)   = Graph (IntMap.insert (id n) (nodeCtx n) ns) es
    delNode n g@(Graph ns es) = Graph ns'' es'
        where nid   = id n
              ns''  = IntMap.delete nid ns'
              mnode = IntMap.lookup nid ns
              updateEdges (NodeCtx ins outs _) = foldr delEdge g (fmap node $ IntSet.elems (IntSet.union ins outs))
              Graph ns' es' = maybe g updateEdges mnode
    insEdge edge (Graph ns es) = Graph ns' es'
        where eid = id edge
              n   = start edge
              m   = end   edge
              ns' = IntMap.adjust (\(NodeCtx x y a) -> NodeCtx (IntSet.insert eid x) y a) m
                  $ IntMap.adjust (\(NodeCtx x y a) -> NodeCtx x (IntSet.insert eid y) a) n
                  $ ns
              es' = IntMap.insert eid (EdgeCtx n m edge) es
    delEdge edge (Graph ns es) = Graph ns' es'
        where eid   = id edge
              medge = IntMap.lookup eid es
              es'   = IntMap.delete eid es
              ns'   = maybe ns updateNodes medge
              updateNodes (EdgeCtx n m _) = IntMap.adjust (\(NodeCtx ins outs a) -> NodeCtx (IntSet.delete eid ins) outs a) m
                                          $ IntMap.adjust (\(NodeCtx ins outs a) -> NodeCtx ins (IntSet.delete eid outs) a) n
                                          $ ns

instance Default (Graph n e) where
    def = Graph def def



newtype Node = Node Int deriving (Show)
node = Node

data Binded a = Binded Int Int a deriving (Show)

type Edge = Binded Node
edge id start end = Binded start end (Node id)

newtype Labeled l a = Labeled (l,a) deriving (Show)
labeled l a = Labeled (l,a)
label (Labeled (l,_)) = l

instance Element a => Element (Labeled l a) where
    id (Labeled (_, a)) = id a

instance Binding a => Binding (Labeled l a) where
    start (Labeled (_, a)) = start a
    end   (Labeled (_, a)) = end   a

instance Element Node where
    id (Node a) = a

instance Element a => Element (Binded a) where
    id (Binded _ _ a) = id a

instance Binding (Binded a) where
    start (Binded s _ _) = s
    end   (Binded _ e _) = e

main = do
    args <- getArgs
    let num  = read (args!!0) :: Int
    let num2 = read (args!!1) :: Int
    --print num
    let
        g = (def :: Graph (Labeled () Node) Edge)
          & insNode (labeled () $ node 0)
          & insNode (labeled () $ node 1)
          & insNode (labeled () $ node 2)
          & insNode (labeled () $ node 3)
          & insEdge (edge 0 1 2)
          --  -- & delEdge 0


    let g1 = foldr insNode (def :: Graph Node Edge) (fmap node [0..num])
        g2 = foldr (\(a,b,c) -> insEdge $ edge a b c) g1 (zip3 [0..] [0..num-1] [1..num])
        g' = foldr delNode g2 (fmap node [0..num2])

    print g
