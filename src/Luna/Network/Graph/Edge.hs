---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Graph.Edge(
Edge(..),
EdgeCls(..),
noEdges,
compareSrc,
compareDst,
sortBySrcL,
sortBydstL
) where

import qualified Data.Graph.Inductive    as DG
import qualified Data.Serialize          as Serialize
import           Data.Serialize            (Serialize)
import           Data.List                 (sortBy)
import           Control.Monad             (liftM)


--import Data.GraphViz.Attributes (Labellable, toLabelValue)
    
data EdgeCls = Standard | Arrow deriving (Show, Read, Ord, Eq)

noEdges :: [Edge]
noEdges = [] 

data Edge = Edge { 
    src :: Int,
    dst :: Int,
    cls :: EdgeCls
} deriving (Show, Read, Ord, Eq)

sortBySrcL :: [DG.LEdge Edge] -> [DG.LEdge Edge]
sortBySrcL edges = sortBy sortf edges where
    sortf (_,_,e1) (_,_,e2) = compareSrc e1 e2

sortBydstL :: [DG.LEdge Edge] -> [DG.LEdge Edge]
sortBydstL edges = sortBy sortf edges where
    sortf (_,_,e1) (_,_,e2) = compareDst e1 e2

compareSrc :: Edge -> Edge -> Ordering
compareSrc (Edge src1 _ _) (Edge src2 _ _) = compare src1 src2

compareDst :: Edge -> Edge -> Ordering
compareDst (Edge _ dst1 _) (Edge _ dst2 _) = compare dst1 dst2


------------------------- INSTANCES -------------------------

instance Serialize EdgeCls where
  put i = Serialize.put $ show i
  get   = liftM (read :: String -> EdgeCls) (Serialize.get :: Serialize.Get String)


instance Serialize Edge where
  put i = Serialize.put (src i, dst i, cls i)
  get   = do
            (src', dst', cls') <- Serialize.get
            return $ Edge src' dst' cls'
