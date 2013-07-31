---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Serialization.Defs where

import           Data.Int
import           Data.HashTable
import qualified Data.Text.Lazy as Text
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)

import qualified Defs_Types
import qualified Luna.Network.Graph.Graph as Graph
import           Luna.Network.Graph.Graph   (Graph)
import qualified Luna.Network.Def.NodeDef as NodeDef
import           Luna.Network.Def.NodeDef   (NodeDef(..))
import           Luna.Network.Path.Import   (Import)
import qualified Luna.Type.Type           as Type
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Attrs ()


instance Serialize (Int, NodeDef) (Defs_Types.NodeDefinition, Graph) where
  encode (defID, NodeDef cls graph imports flags attributes libID) = (tdef, graph) where
     ttype       = Nothing -- TODO [PM] : make work here: Just $ encode cls
     tflags      = Just $ encode flags
     tattributes = Just $ encode attributes
     tlibID      = Just $ hashInt libID
     tdefID      = Just $ hashInt defID
     tdef = Defs_Types.NodeDefinition ttype tflags tattributes tlibID tdefID 
  decode td = case td of 
     (Defs_Types.NodeDefinition Nothing (Just tflags) (Just tattributes) (Just tlibID) (Just tdefID), graph)
           -> d where
                    d = case (decode tflags, decode tattributes) of
                        (Right flags, Right attributes)
                               -> let imports = [] :: [Import]
                                      libID = (fromInteger. toInteger::Int32 -> Int) tlibID
                                      nodeDef = NodeDef Type.Undefined graph imports flags attributes libID
                                      defID = 1
                                  in Right (defID, nodeDef)
                        (Right flags, Left message) 
                               -> Left $ "Failed to deserialize attributes : " ++ message
                        (Left message, _) 
                               -> Left $ "Failed to deserialize flags : " ++ message
     (Defs_Types.NodeDefinition {}, _) -> Left "Some fields are missing"

