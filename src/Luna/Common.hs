---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Common(
Graph   (..),
Node    (..),
NodeDef (..),
) where


import qualified Data.Graph.Inductive as DG
import           Luna.Edge 			        (Edge)
import           Luna.DefaultValue 	    (DefaultValue)
import           Data.Map               (Map)
import qualified Data.MultiMap        as MultiMap
import           Data.MultiMap          (MultiMap)
import qualified Luna.Library         as Library

import qualified Data.Serialize       as Serialize
import           Data.Serialize         (Serialize)
import           Data.Word              (Word8)

data Graph = Graph {
	repr      :: DG.Gr Node Edge,
  children  :: MultiMap String DG.Node,
	types     :: Map 	  String DG.Node,
	calls     :: Map 	  String DG.Node,
	classes   :: Map 	  String DG.Node,
	functions :: MultiMap String DG.Node,
	packages  :: Map 	  String DG.Node
} deriving (Show)

-----------------------------------------------------------------

data Node = TypeNode    { name  :: String }
		  | CallNode    { name  :: String }
		  | ClassNode   { name  :: String, def :: NodeDef }
		  | FunctionNode{ name  :: String, def :: NodeDef }
		  | PackageNode { name  :: String, def :: NodeDef }
		  | DefaultNode { value :: DefaultValue }
		  deriving (Show)


-- FIXME[wd] move the following instance to the right place
instance (Show k, Show a) => Show (MultiMap k a) where
    show a = show $ MultiMap.toMap a


-----------------------------------------------------------------

data NodeDef = NotLoaded
	         | NodeDef {
			       inputs 	:: [String],
				   outputs  :: [String],
				   imports  :: [String],
				   graph	:: Graph,
				   libID	:: Library.LibID
			   } deriving (Show)

------------------------- INSTANCES -------------------------

instance Serialize Graph where
  put i = Serialize.put (repr i, MultiMap.toMap $ children i, types i, calls i, classes i, MultiMap.toMap $ functions i, packages i)
  get   = do 
            (repr', children', types', calls', classes', functions', packages') <- Serialize.get
            return $ Graph repr' (MultiMap.fromMap children') types' calls' classes' (MultiMap.fromMap functions') packages'


instance (Serialize a, Serialize b) => Serialize (DG.Gr a b) where
  put i = Serialize.put (DG.labNodes i, DG.labEdges i)
  get = do
          (nd, edg) <- Serialize.get
          return $ DG.mkGraph nd edg

--------------------------------------------------------------

instance Serialize Node where
  put i = case i of 
            TypeNode     name'    -> Serialize.put (0 :: Word8, name')
            CallNode     name'    -> Serialize.put (1 :: Word8, name')
            ClassNode    name'  _ -> Serialize.put (2 :: Word8, name')
            FunctionNode name'  _ -> Serialize.put (3 :: Word8, name')
            PackageNode  name'  _ -> Serialize.put (4 :: Word8, name')
            DefaultNode  value'   -> Serialize.put (5 :: Word8, value')

  get   = do 
            t <- Serialize.get :: Serialize.Get Word8
            case t of 
              0 -> do name'       <- Serialize.get; return $ TypeNode     name'
              1 -> do name'       <- Serialize.get; return $ CallNode     name'
              2 -> do name'       <- Serialize.get; return $ ClassNode    name'  NotLoaded
              3 -> do name'       <- Serialize.get; return $ FunctionNode name'  NotLoaded
              4 -> do name'       <- Serialize.get; return $ PackageNode  name'  NotLoaded
              5 -> do value'      <- Serialize.get; return $ DefaultNode  value'
              _ -> error "Unknown Node Type (unserialize)"

--------------------------------------------------------------

instance Serialize NodeDef where
  put i = Serialize.put (inputs i, outputs i, imports i, graph i, libID i)
  get   = do
            (inputs', outputs', imports', graph', libID') <- Serialize.get
            return $ NodeDef inputs' outputs' imports' graph' libID'
