---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Types () where

import           Flowbox.Prelude                   
import           Data.Text.Lazy                    (pack, unpack)
import qualified Data.Vector                     as Vector
import qualified Types_Types                     as TTypes
import           Flowbox.Control.Error             
import qualified Flowbox.Luna.XOLD.Type.Type     as Type
import           Flowbox.Luna.XOLD.Type.Type       (Type)
import           Flowbox.Tools.Conversion.Thrift   


typeList2typeProtoList :: Int -> [Type] -> ([Int], [TTypes.TypeProto])
typeList2typeProtoList level types = case types of 
    []    -> ([], [])
    [h]   -> ([level], type2typeProtoList level h)
    (h:t) -> (levelList, protoList) where 
                hproto = type2typeProtoList level h

                tlevel = level + (length hproto)
                (tlevels, tprotos) = typeList2typeProtoList tlevel t
                
                levelList = level:tlevels
                protoList = hproto++tprotos


type2typeProtoList :: Int -> Type -> [TTypes.TypeProto]
type2typeProtoList level t = case t of 
    Type.Undefined       -> [tcurrent] where
       tcls       = Just TTypes.Undefined
       tcurrent   = TTypes.TypeProto tcls Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Type.Module name fields -> tcurrent : tfields where
       tcls       = Just TTypes.Module
       tname      = Just $ pack name
       fieldsLevel   = level + 1
       (tfieldsLevels', tfields) = typeList2typeProtoList fieldsLevel fields
       tfieldsLevels = Just $ Vector.fromList $ map (itoi32) tfieldsLevels'
       tcurrent   = TTypes.TypeProto tcls tname   Nothing Nothing tfieldsLevels Nothing Nothing Nothing
    Type.Function name ainputs aoutputs -> tcurrent : tinputs ++ toutputs where
       tcls          = Just TTypes.Function
       tname         = Just $ pack name

       inputsLevel   = level + 1
       tinputsLevel  = Just $ itoi32 inputsLevel
       tinputs       = type2typeProtoList inputsLevel ainputs
       
       outputsLevel  = inputsLevel + (length tinputs)
       toutputsLevel = Just $ itoi32 outputsLevel
       toutputs      = type2typeProtoList outputsLevel aoutputs
       
       tcurrent      = TTypes.TypeProto tcls tname   Nothing Nothing Nothing tinputsLevel toutputsLevel Nothing
    Type.Tuple aitems     -> tcurrent : titems where
       tcls          = Just TTypes.Tuple
       itemsLevel    = level + 1
       (itemsLevels', titems) = typeList2typeProtoList itemsLevel aitems
       titemsLevels  = Just $ Vector.fromList $ map (itoi32) itemsLevels'
       tcurrent      = TTypes.TypeProto tcls Nothing titemsLevels Nothing Nothing Nothing Nothing Nothing
    Type.Class name atypeparams fields -> tcurrent : tfields where
       tcls          = Just TTypes.Class
       tname         = Just $ pack name
       ttypeparams   = Just $ Vector.fromList $ map (pack) atypeparams
       fieldsLevel   = level + 1
       (tfieldsLevels', tfields) = typeList2typeProtoList fieldsLevel fields
       tfieldsLevels = Just $ Vector.fromList $ map (itoi32) tfieldsLevels'
       tcurrent      = TTypes.TypeProto tcls tname Nothing ttypeparams tfieldsLevels  Nothing Nothing Nothing
    Type.Named name atype -> tcurrent:ttype where
       tcls       = Just TTypes.Named
       typeLevel  = level + 1
       ttypeLevel = Just $ itoi32 typeLevel
       tname      = Just $ pack name
       tcurrent   = TTypes.TypeProto tcls tname Nothing Nothing Nothing Nothing Nothing ttypeLevel
       
       ttype      = type2typeProtoList (level+1) atype
    Type.TypeName name -> [tcurrent] where
       tcls       = Just TTypes.TypeName
       tname      = Just $ pack name
       
       tcurrent   = TTypes.TypeProto tcls tname   Nothing Nothing Nothing Nothing Nothing Nothing 


typeFromListAt :: [TTypes.TypeProto] -> Int -> Either String Type
typeFromListAt list index = t where
    (TTypes.TypeProto mtcls mtname mtitemsInds mttypeparams mtfieldsInds mtinputsIndex mtoutputsIndex mttypeIndex) = list !! index
    t = case mtcls of 
        Just TTypes.Undefined -> Right Type.Undefined
        Just TTypes.Module    -> do 
            tname       <- mtname       <?> "Failed to decode Type: `name` field is missing"
            tfieldsInds <- mtfieldsInds <?> "Failed to decode Type: `params` field is missing"
            let fieldsInds = map (i32toi) $ Vector.toList tfieldsInds
            fields         <- mapM (typeFromListAt list) fieldsInds
            return $ Type.Module (unpack tname) fields
        Just TTypes.Function  -> do 
            tname           <- mtname         <?> "Failed to decode Type: `name` field is missing"
            tinputsIndex    <- mtinputsIndex  <?> "Failed to decode Type: `inputs` field is missing"
            toutputsIndex   <- mtoutputsIndex <?> "Failed to decode Type: `outputs` field is missing"
            ainputs         <- typeFromListAt list $ i32toi tinputsIndex
            aoutputs        <- typeFromListAt list $ i32toi toutputsIndex
            return $ Type.Function (unpack tname) ainputs aoutputs
        Just TTypes.Tuple -> do 
            titemsInds      <- mtitemsInds <?> "Failed to decode Type: `items` field is missing"
            let itemsInds   = map (i32toi) $ Vector.toList titemsInds
            items           <- mapM (typeFromListAt list) itemsInds
            return $ Type.Tuple items
        Just TTypes.Class -> do 
            tname           <- mtname       <?> "Failed to decode Type: `name` field is missing"
            ttypeparams     <- mttypeparams <?> "Failed to decode Type: `typeparams` field is missing"
            tfieldsInds     <- mtfieldsInds <?> "Failed to decode Type: `params` field is missing"
            let typeparams = map (unpack) $ Vector.toList ttypeparams
                fieldsInds = map (i32toi) $ Vector.toList tfieldsInds
            fields        <- mapM (typeFromListAt list) fieldsInds
            return $ Type.Class (unpack tname) typeparams fields
        Just TTypes.Named  -> do
            tname           <- mtname <?>  "Failed to decode Type: `name` field is missing"
            ttypeIndex      <- mttypeIndex <?> "Failed to decode Type: `type` field is missing"
            internal        <- typeFromListAt list (i32toi ttypeIndex)
            return $ Type.Named (unpack tname) internal
        Just TTypes.TypeName -> do 
            tname <- mtname <?> "Failed to decode Type: `name` field is missing" 
            return $ Type.TypeName $ unpack tname
        Nothing                    -> Left "Failed to decode Type: `cls` field is missing"


instance Convert Type TTypes.Type where
    encode t = tt where list = type2typeProtoList 0 t
                        tt = TTypes.Type $ Just $ Vector.fromList list
    decode (TTypes.Type mvector) = do 
        vector <- mvector <?> "Failed to decode Type: `types` field is missing"
        let list = Vector.toList vector
        typeFromListAt list 0



instance Convert [Type] [TTypes.Type] where
  encode t  = map (encode) t
  decode tt = mapM (decode) tt
 