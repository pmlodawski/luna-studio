---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs where

import           Data.Int                                                    
import           Data.Text.Lazy                                              (pack, unpack)
import qualified Data.HashMap.Strict                                       as HashMap
import qualified Data.Vector                                               as Vector

import qualified Defs_Types                                                as TDefs
import           Flowbox.Luna.Network.Graph.Graph                            (Graph)
import           Flowbox.Luna.Network.Def.Edge                               (Edge(..))
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition(..))
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager(..))
import           Flowbox.Luna.Network.Path.Import                            (Import(..))
import qualified Flowbox.Luna.Network.Path.Path                            as Path
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Attrs        ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Types        ()
import           Flowbox.Tools.Conversion  


encodeLabNode :: (Definition.ID, Definition) -> (Int32, TDefs.Definition)
encodeLabNode node@(defID, _) = tnode where
    tnode = (itoi32 defID, encodeDef node)

    encodeDef d = td where
        (td, _) = encode d


instance Convert (Int, Int, Edge) TDefs.Edge where
    encode (asrc, adst, _) = tedge where
        tedge = TDefs.Edge (Just $ itoi32 asrc) (Just $ itoi32 adst)
    decode = error "Not implemented" --TODO [PM] not implemented


instance Convert DefManager TDefs.DefsGraph where
    encode defManager = tdefGraph where
        labNodesList = DefManager.labNodes defManager
        tdefs        = HashMap.fromList $ map (encodeLabNode) labNodesList

        labEdgesList = DefManager.labEdges defManager
        tedges       = Vector.fromList $ map (encode) labEdgesList
        tdefGraph    = TDefs.DefsGraph (Just tdefs) (Just tedges)
    decode tdefGraph = defManager where
        defManager = error "Not implemented" --TODO [PM] not implemented
        

instance Convert Import TDefs.Import where
    encode (Import apath aitems) = timport where
        tpath   = Just $ Vector.fromList $ map (pack) $ Path.toList apath
        titems  = Just $ Vector.fromList $ map (pack) aitems
        timport = TDefs.Import tpath titems
    decode timport = case timport of 
        TDefs.Import (Just tpath) (Just titems) -> Right $ Import apath aitems where
                                                        apath = Path.fromList $ map (unpack) $ Vector.toList tpath
                                                        aitems = map (unpack) $ Vector.toList titems
        TDefs.Import (Just _    ) Nothing       -> Left "`items` field is missing"
        TDefs.Import Nothing      _             -> Left "`path` field is missing"



instance Convert [Import] TDefs.Imports where
    encode aimports = Vector.fromList $ map (encode) aimports
    decode timports = aimports where
        timportsList = Vector.toList timports
        imports1 = map (decode :: TDefs.Import -> Either String Import) timportsList
        aimports = convert imports1


instance Convert (Int, Definition) (TDefs.Definition, Graph) where
  encode (defID, Definition acls agraph aimports aflags aattributes) = (tdef, agraph) where
     ttype       = Just $ encode acls
     timports    = Just $ encode aimports
     tflags      = Just $ encode aflags
     tattributes = Just $ encode aattributes
     tdefID      = Just $ itoi32 defID
     tdef = TDefs.Definition ttype timports tflags tattributes tdefID 
  decode (TDefs.Definition mtcls mtimports mtflags mtattributes mtdefID, agraph) = case mtcls of 
    Nothing                                           -> Left "`type` field is missing"
    Just tcls                                         -> case mtimports of 
        Nothing                                       -> Left "`imports` field is missing"
        Just timports                                 -> case mtflags of 
            Nothing                                   -> Left "`flags` field is missing"
            Just tflags                               -> case mtattributes of 
                Nothing                               -> Left "`attributes` field is missing"
                Just tattributes                      -> case mtdefID of 
                    Nothing                           -> Left "`defID` field is missing"
                    Just tdefID                       -> case decode tcls of 
                        Left message                  -> Left $ "Failed to deserialize `cls` : " ++ message
                        Right acls                    -> case decode timports of 
                            Left message              -> Left $ "Failed to deserialize `imports` : " ++ message
                            Right aimports            -> case decode tflags of 
                                Left message          -> Left $ "Failed to deserialize `flags` : " ++ message
                                Right aflags          -> case decode tattributes of
                                    Left message      -> Left $ "Failed to deserialize `attributes` : " ++ message
                                    Right aattributes -> Right (adefID, nodeDef) where
                                        nodeDef = Definition acls agraph aimports aflags aattributes
                                        adefID = i32toi tdefID



