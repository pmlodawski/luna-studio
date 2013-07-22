---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Graph.Node(
Node(..)
) where

import           Luna.Network.Graph.DefaultValue   (DefaultValue)
import qualified Luna.Network.Flags              as Flags
import           Luna.Network.Flags                (Flags)
import qualified Luna.Network.Attributes         as Attributes
import           Luna.Network.Attributes           (Attributes)

type NodeDefID = Int

data Node = TypeNode     { name :: String, flags :: Flags, attributes :: Attributes }
          | CallNode     { name :: String, flags :: Flags, attributes :: Attributes }
          | ClassNode    { did :: NodeDefID }
          | FunctionNode { did :: NodeDefID }
          | PackageNode  { did :: NodeDefID }
          | DefaultNode  { value :: DefaultValue }
          deriving (Show)

type ID     = Int

------------------------- INSTANCES -------------------------

--instance Serialize Node where
--  put i = case i of x
--            TypeNode     name'    -> Serialize.put (0 :: Word8, name')
--            CallNode     name'    -> Serialize.put (1 :: Word8, name')
--            ClassNode    name'  _ -> Serialize.put (2 :: Word8, name')
--            FunctionNode name'  _ -> Serialize.put (3 :: Word8, name')
--            PackageNode  name'  _ -> Serialize.put (4 :: Word8, name')
--            DefaultNode  value'   -> Serialize.put (5 :: Word8, value')
--  get   = do 
--            t <- Serialize.get :: Serialize.Get Word8
--            case t of 
--              0 -> do name'       <- Serialize.get; return $ TypeNode     name'
--              1 -> do name'       <- Serialize.get; return $ CallNode     name'
--              2 -> do name'       <- Serialize.get; return $ ClassNode    name'  NotLoaded
--              3 -> do name'       <- Serialize.get; return $ FunctionNode name'  NotLoaded
--              4 -> do name'       <- Serialize.get; return $ PackageNode  name'  NotLoaded
--              5 -> do value'      <- Serialize.get; return $ DefaultNode  value'
--              _ -> error "Unknown Node Type (unserialize)"
