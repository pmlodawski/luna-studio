---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Edge(
Edge(..),
EdgeCls(..),
noEdges
) where

import qualified Data.Serialize       as Serialize
import           Data.Serialize         (Serialize)

import Control.Monad(liftM)
import Data.GraphViz.Attributes (Labellable, toLabelValue)
	
data EdgeCls = Standard | Arrow deriving (Show, Read, Ord, Eq)

noEdges :: [Edge]
noEdges = [] 

data Edge = Edge { 
	source :: String,
	target :: String,
	cls :: EdgeCls
} deriving (Show, Read, Ord, Eq)

instance Labellable Edge where
	toLabelValue = toLabelValue . show

------------------------- INSTANCES -------------------------

instance Serialize EdgeCls where
  put i = Serialize.put $ show i
  get   = liftM (read :: String -> EdgeCls) (Serialize.get :: Serialize.Get String)


instance Serialize Edge where
  put i = Serialize.put (source i, target i, cls i)
  get   = do
            (source', target', cls') <- Serialize.get
            return $ Edge source' target' cls'
