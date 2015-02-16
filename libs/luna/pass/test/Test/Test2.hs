
import Data.Default
import Data.IntMap (IntMap)
import Control.Lens
import qualified Data.IntMap as IntMap
import qualified Data.IntSet    as IntSet
import           Data.IntSet    (IntSet)
import           Data.Monoid
import System.Environment (getArgs)

import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.Graph (insNode, insEdge, Edge, UEdge)
import Data.Graph.Inductive.PatriciaTree


main = do
    args <- getArgs
    let num  = read (args!!0) :: Int
    let num2 = read (args!!1) :: Int
    print num


    let g1 = foldr Graph.insNode (Graph.empty :: Gr () ()) (zip [0..num] $ repeat ())
        --g2 = foldr Graph.insEdge g1 (zip3 [0..num-1] [1..num] $ repeat ())
        --g3 = foldr Graph.delEdge g2 (reverse (zip  [0..num-1] [1..num]))
        g' = foldr Graph.delNode g1 [0..num2]

    print g'