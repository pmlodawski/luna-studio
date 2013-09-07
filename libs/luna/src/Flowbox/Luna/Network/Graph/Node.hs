---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Graph.Node(
    Node(..),
    ID,
    mkExpr,
    mkInputs,
    mkOutputs,
    mkTuple,
    mkNTuple,
) where

import           Flowbox.Prelude

import           Flowbox.Luna.Network.Graph.DefaultValue   (DefaultValue)
import qualified Flowbox.Luna.Network.Flags              as Flags
import           Flowbox.Luna.Network.Flags                (Flags)
import qualified Flowbox.Luna.Network.Attributes         as Attributes
import           Flowbox.Luna.Network.Attributes           (Attributes)

--type NodeDefID = Int

data Node = Expr     { expression :: String, flags :: Flags, attributes :: Attributes }
          | Default  { value :: DefaultValue, attributes :: Attributes}
          | Inputs   { flags :: Flags, attributes :: Attributes }
          | Outputs  { flags :: Flags, attributes :: Attributes }
          | Tuple    { flags :: Flags, attributes :: Attributes }
          | NTuple   { flags :: Flags, attributes :: Attributes }
          
          deriving (Show)

type ID     = Int

mkExpr :: String -> Node
mkExpr name = Expr name Flags.empty Attributes.empty

mkInputs :: Node
mkInputs = Inputs Flags.empty Attributes.empty

mkOutputs :: Node
mkOutputs = Outputs Flags.empty Attributes.empty

mkTuple :: Node
mkTuple = Tuple Flags.empty Attributes.empty

mkNTuple :: Node
mkNTuple = NTuple Flags.empty Attributes.empty

 
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
