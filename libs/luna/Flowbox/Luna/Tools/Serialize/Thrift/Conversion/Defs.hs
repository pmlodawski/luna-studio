---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs where


import qualified Data.Text.Lazy as Text
import qualified Data.Vector    as Vector

import qualified Defs_Types               as TDefs
import           Flowbox.Luna.Network.Graph.Graph   (Graph)
import           Flowbox.Luna.Network.Def.Definition   (Definition(..))
import           Flowbox.Luna.Network.Path.Import   (Import(..))
import qualified Flowbox.Luna.Network.Path.Path   as Path
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Attrs ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Types ()

 
instance Convert Import TDefs.Import where
    encode (Import apath aitems) = timport where
        tpath   = Just $ Vector.fromList $ map (Text.pack) $ Path.toList apath
        titems  = Just $ Vector.fromList $ map (Text.pack) aitems
        timport = TDefs.Import tpath titems
    decode timport = case timport of 
        TDefs.Import (Just tpath) (Just titems) -> Right $ Import apath aitems where
                                                        apath = Path.fromList $ map (Text.unpack) $ Vector.toList tpath
                                                        aitems = map (Text.unpack) $ Vector.toList titems
        TDefs.Import (Just _    ) Nothing       -> Left "`items` field is missing"
        TDefs.Import Nothing      _             -> Left "`path` field is missing"



instance Convert [Import] TDefs.Imports where
    encode aimports = Vector.fromList $ map (encode) aimports
    decode timports = aimports where
        timportsList = Vector.toList timports
        imports1 = map (decode :: TDefs.Import -> Either String Import) timportsList
        aimports = convert imports1


instance Convert (Int, Definition) (TDefs.Definition, Graph) where
  encode (defID, Definition acls agraph aimports aflags aattributes alibID) = (tdef, agraph) where
     ttype       = Just $ encode acls
     timports    = Just $ encode aimports
     tflags      = Just $ encode aflags
     tattributes = Just $ encode aattributes
     tlibID      = Just $ itoi32 alibID
     tdefID      = Just $ itoi32 defID
     tdef = TDefs.Definition ttype timports tflags tattributes tlibID tdefID 
  decode td = case td of 
     (TDefs.Definition (Just tcls) (Just timports) (Just tflags) (Just tattributes) (Just tlibID) (Just tdefID), agraph)
           -> d where
                    d = case (decode tcls, decode timports, decode tflags, decode tattributes) of
                        (Right acls, Right aimports, Right aflags, Right aattributes)
                               -> Right (adefID, nodeDef) where
                                  alibID = i32toi tlibID
                                  nodeDef = Definition acls agraph aimports aflags aattributes alibID
                                  adefID = i32toi tdefID
                        (Right _   , Right _      , Right _     , Left message) 
                               -> Left $ "Failed to deserialize `attributes` : " ++ message
                        (Right _   , Right _      , Left message, _           ) 
                               -> Left $ "Failed to deserialize `flags` : " ++ message
                        (Right _   , Left message , _           , _           )
                               -> Left $ "Failed to deserialize `imports` : " ++ message
                        (Left message, _          , _           , _           )
                               -> Left $ "Failed to deserialize `cls` : " ++ message
     (TDefs.Definition (Just _) (Just _) (Just _) (Just _) (Just _) Nothing, _) -> Left "`defID` field is missing"
     (TDefs.Definition (Just _) (Just _) (Just _) (Just _) Nothing  _      , _) -> Left "`libID` field is missing"
     (TDefs.Definition (Just _) (Just _) (Just _) Nothing  _        _      , _) -> Left "`attributes` field is missing"
     (TDefs.Definition (Just _) (Just _) Nothing  _        _        _      , _) -> Left "`flags` field is missing"
     (TDefs.Definition (Just _) Nothing  _        _        _        _      , _) -> Left "`imports` field is missing"
     (TDefs.Definition Nothing  _        _        _        _        _      , _) -> Left "`type` field is missing"



