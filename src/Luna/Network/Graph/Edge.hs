---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Graph.Edge(
Edge(..),
EdgeCls(..),
noEdges
) where

import qualified Data.Serialize       as Serialize
import           Data.Serialize         (Serialize)


import Control.Monad(liftM)
--import Data.GraphViz.Attributes (Labellable, toLabelValue)
    
data EdgeCls = Standard | Arrow deriving (Show, Read, Ord, Eq)

noEdges :: [Edge]
noEdges = [] 

data Edge = Edge { 
    src :: Int,
    dst :: Int,
    cls :: EdgeCls
} deriving (Show, Read, Ord, Eq)

------------------------- INSTANCES -------------------------

instance Serialize EdgeCls where
  put i = Serialize.put $ show i
  get   = liftM (read :: String -> EdgeCls) (Serialize.get :: Serialize.Get String)


instance Serialize Edge where
  put i = Serialize.put (src i, dst i, cls i)
  get   = do
            (src', dst', cls') <- Serialize.get
            return $ Edge src' dst' cls'
