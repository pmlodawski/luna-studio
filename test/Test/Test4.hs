
import Data.Default
import Data.IntMap (IntMap)
import Control.Lens
import qualified Data.IntMap as IntMap
import qualified Data.IntSet    as IntSet
import           Data.IntSet    (IntSet)
import           Data.Monoid
import System.Environment (getArgs)

type Edge = (Int, Int)


newtype Graph = Graph (IntMap (IntSet,IntSet), IntMap Edge)
              deriving (Show)

instance Default Graph where
    def = Graph def


-- O(min(n,W))
insNode n (Graph (ns,es)) = ns' `seq` Graph (ns',es)
  where ns' = IntMap.insert n (mempty,mempty) ns

-- O(min(n,W))
delNode n g@(Graph (ns,es)) = Graph (ns',es)
    where ns'   = IntMap.delete n ns
          mnode = IntMap.lookup n ns
          updateEdges (ins, outs) = foldr delEdge g (IntSet.elems (IntSet.union ins outs))
          g' = maybe g updateEdges mnode

-- O(min(n,W))
insEdge e n m (Graph (ns, es)) = (ns', es') `seq` Graph (ns', es')
    where ns' = IntMap.adjust (\(x,y) -> (IntSet.insert e x, y)) m
              $ IntMap.adjust (\(x,y) -> (x, IntSet.insert e y)) n
              $ ns
          es' = IntMap.insert e (n,m) es

---- O(min(n,W))
delEdge e (Graph ns es) = Graph ns' es'
    where medge = IntMap.lookup e es
          es'   = IntMap.delete e es
          ns'   = maybe ns updateNodes medge
          updateNodes (n, m) = IntMap.adjust (\(ins,outs) -> (IntSet.delete e ins, outs)) m
                             $ IntMap.adjust (\(ins,outs) -> (ins, IntSet.delete e outs)) n
                             $ ns 

----subgraph nodes (Graph ns es) 

main = do
    args <- getArgs
    let num  = read (args!!0) :: Int
    let num2 = read (args!!1) :: Int
    print num
    --let g = def
    --      & insNode 0
    --      & insNode 1
    --      & insNode 2
    --      & insEdge 0 0 1
    --      -- & delEdge 0
    --      & delNode 0

    let g1 = foldr insNode def [0..num]
        g2 = foldr (\(a,b,c) -> insEdge a b c) g1 (zip3 [0..num-1] [0..num-1] [1..num])
        --g3 = foldr delEdge g2 [0..num-1]
        g' = foldr delNode g2 [0..num2]

    print g'