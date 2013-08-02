---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Serialization.Defs where


import qualified Data.Text.Lazy as Text
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)

import qualified Defs_Types
import qualified Luna.Network.Graph.Graph as Graph
import           Luna.Network.Graph.Graph   (Graph)
import qualified Luna.Network.Def.NodeDef as NodeDef
import           Luna.Network.Def.NodeDef   (NodeDef(..))
import qualified Luna.Network.Path.Import as Import
import           Luna.Network.Path.Import   (Import(..))
import qualified Luna.Network.Path.Path   as Path
import qualified Luna.Type.Type           as Type
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Attrs ()


instance Serialize Import Defs_Types.Import where
    encode (Import path items) = timport where
        tpath   = Just $ Vector.fromList $ map (Text.pack) $ Path.toList path
        titems  = Just $ Vector.fromList $ map (Text.pack) items
        timport = Defs_Types.Import tpath titems
    decode timport = case timport of 
        Defs_Types.Import (Just tpath) (Just titems) -> Right $ Import path items where
                                                        path = Path.fromList $ map (Text.unpack) $ Vector.toList tpath
                                                        items = map (Text.unpack) $ Vector.toList titems
        Defs_Types.Import (Just tpath) Nothing       -> Left "`items` field is missing"
        Defs_Types.Import Nothing      Nothing       -> Left "`path` field is missing"



instance Serialize [Import] Defs_Types.Imports where
    encode imports = Vector.fromList $ map (encode) imports
    decode timports = imports where
        timportsList = Vector.toList timports
        imports1 = map (decode :: Defs_Types.Import -> Either String Import) timportsList
        imports = convert imports1


instance Serialize (Int, NodeDef) (Defs_Types.NodeDef, Graph) where
  encode (defID, NodeDef cls graph imports flags attributes libID) = (tdef, graph) where
     ttype       = Nothing -- TODO [PM] : make work here: Just $ encode cls
     timports    = Just $ encode imports
     tflags      = Just $ encode flags
     tattributes = Just $ encode attributes
     tlibID      = Just $ itoi32 libID
     tdefID      = Just $ itoi32 defID
     tdef = Defs_Types.NodeDef ttype timports tflags tattributes tlibID tdefID 
  decode td = case td of 
     (Defs_Types.NodeDef Nothing (Just timports) (Just tflags) (Just tattributes) (Just tlibID) (Just tdefID), graph)
           -> d where
                    d = case (decode timports :: Either String [Import], decode tflags, decode tattributes) of
                        (Right imports, Right flags, Right attributes)
                               -> let libID = i32toi tlibID
                                      nodeDef = NodeDef Type.Undefined graph imports flags attributes libID
                                      defID = 1
                                  in Right (defID, nodeDef)
                        (Right imports, Right flags , Left message) 
                               -> Left $ "Failed to deserialize attributes : " ++ message
                        (Right imports, Left message, _           ) 
                               -> Left $ "Failed to deserialize flags : " ++ message
                        (Left message , _           , _           )
                               -> Left $ "Failed to deserialize imports : " ++ message
     (Defs_Types.NodeDef {}, _) -> Left "Some fields are missing"


convert :: [Either String a] -> Either String [a]
convert []       = Right []
convert [h]      = case h of 
    Left m  -> Left m
    Right a -> Right [a]
convert (h:tail) = case h of 
    Left m  -> Left m
    Right a -> case (convert tail) of 
        Left m1 -> Left m1
        Right a1 -> Right (a:a1)

