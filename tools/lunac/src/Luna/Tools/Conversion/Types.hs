---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Conversion.Types () where


import           Data.Text.Lazy   (pack, unpack)
import qualified Data.Vector    as Vector

import qualified Types_Types              as TTypes
import           Luna.Tools.Conversion
import           Luna.Type.Type             (Type(..))

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
    Undefined       -> [tcurrent] where
                       tcls       = Just TTypes.Undefined
                       tcurrent   = TTypes.TypeProto tcls Nothing Nothing Nothing Nothing Nothing Nothing
    Module aname     -> [tcurrent] where
                       tcls       = Just TTypes.Module
                       tname      = Just $ pack aname
                       
                       tcurrent   = TTypes.TypeProto tcls tname   Nothing Nothing Nothing Nothing Nothing 
    List aitem       -> tcurrent:titem where
                       tcls       = Just TTypes.List
                       itemLevel  = level + 1
                       titemLevel = Just $ itoi32 itemLevel
                       
                       tcurrent   = TTypes.TypeProto tcls Nothing Nothing Nothing Nothing Nothing titemLevel
                       
                       titem      = type2typeProtoList (level+1) aitem
    Function aname ainputs aoutputs 
                    -> tcurrent : tinputs ++ toutputs where
                       tcls          = Just TTypes.Function
                       tname         = Just $ pack aname

                       inputsLevel   = level + 1
                       tinputsLevel  = Just $ itoi32 inputsLevel
                       tinputs       = type2typeProtoList inputsLevel ainputs
                       
                       outputsLevel  = inputsLevel + (length tinputs)
                       toutputsLevel = Just $ itoi32 outputsLevel
                       toutputs      = type2typeProtoList outputsLevel aoutputs
                       
                       tcurrent      = TTypes.TypeProto tcls tname   Nothing Nothing tinputsLevel toutputsLevel Nothing
    Tuple aitems     -> tcurrent : titems where
                       tcls          = Just TTypes.Tuple
                       itemsLevel    = level + 1
                       (itemsLevels', titems) = typeList2typeProtoList itemsLevel aitems
                       titemsLevels  = Just $ Vector.fromList $ map (itoi32) itemsLevels'
                       tcurrent      = TTypes.TypeProto tcls Nothing titemsLevels Nothing Nothing Nothing Nothing
    Class aname aparams -> tcurrent : tparams where
                       tcls          = Just TTypes.Class
                       tname         = Just $ pack aname
                       paramsLevel   = level + 1
                       (tparamsLevels', tparams) = typeList2typeProtoList paramsLevel aparams
                       tparamsLevels = Just $ Vector.fromList $ map (itoi32) tparamsLevels'
                       tcurrent      = TTypes.TypeProto tcls tname Nothing tparamsLevels Nothing Nothing Nothing
    Named aname atype -> tcurrent:ttype where
                       tcls       = Just TTypes.Named
                       typeLevel  = level + 1
                       ttypeLevel = Just $ itoi32 typeLevel
                       tname      = Just $ pack aname
                       tcurrent   = TTypes.TypeProto tcls tname Nothing Nothing Nothing Nothing ttypeLevel
                       
                       ttype      = type2typeProtoList (level+1) atype



typeFromListAt :: [TTypes.TypeProto] -> Int -> Either String Type
typeFromListAt list index = t where
    (TTypes.TypeProto mtcls mtname mtitemsInds mtparamsInds mtinputsIndex mtoutputsIndex mttypeIndex) = list !! index
    t = case mtcls of 
        Just TTypes.Undefined -> Right Undefined
        Just TTypes.Module    -> case mtname of 
                                          Just aname     -> Right $ Module $ unpack aname
                                          Nothing        -> Left  "`name` field is missing"
        Just TTypes.List      -> case mttypeIndex of
                                          Just typeIndex -> do aitems <-  typeFromListAt list $ i32toi typeIndex
                                                               return $ List aitems
                                          Nothing        -> Left  "`type` field is missing"
        Just TTypes.Function  -> case (mtname, mtinputsIndex, mtoutputsIndex) of 
                                          (Just tname, Just tinputsIndex, Just toutputsIndex)
                                                         -> do ainputs  <- typeFromListAt list $ i32toi tinputsIndex
                                                               aoutputs <- typeFromListAt list $ i32toi toutputsIndex
                                                               return $ Function (unpack tname) ainputs aoutputs
                                          (Just _    , Just _          , Nothing)
                                                         -> Left "`outputs` field is missing"
                                          (Just _    , Nothing         , _      )
                                                         -> Left "`inputs` field is missing"
                                          (Nothing   , _               , _      )
                                                         -> Left "`name` field is missing"
        Just TTypes.Tuple     -> case mtitemsInds of
                                          Just titemsInds
                                                         -> do let itemsInds = map (i32toi) $ Vector.toList titemsInds
                                                                   aitems = map (typeFromListAt list) itemsInds
                                                               aitems' <- convert aitems
                                                               return $ Tuple aitems'
                                          Nothing        -> Left "`items` field is missing"
        Just TTypes.Class     -> case (mtname, mtparamsInds) of
                                          (Just tname, Just tparamsInds32)
                                                         -> do let paramsInds = map (i32toi) $ Vector.toList tparamsInds32
                                                                   aparams = map (typeFromListAt list) paramsInds
                                                               aparams' <- convert aparams
                                                               return $ Class (unpack tname) aparams'
                                          (Just _    , Nothing)
                                                         -> Left "`params` field is missing"
                                          (Nothing   , _      )
                                                         -> Left "`name` field is missing"
        Just TTypes.Named     -> case (mtname, mttypeIndex) of 
                                          (Just tname, Just ttypeIndex)
                                                         -> do internal <- typeFromListAt list (i32toi ttypeIndex)
                                                               return $ Named (unpack tname) internal
                                          (Just _    , Nothing)
                                                        -> Left "`type` field is missing"
                                          (Nothing   , _     )
                                                        -> Left "`name` field is missing"
        Nothing                    -> Left "`cls` field is missing"
        _                          -> Left "Unsupported `cls` (not implemented)"


instance Convert Type TTypes.Type where
  encode t = tt where list = type2typeProtoList 0 t
                      tt = TTypes.Type $ Just $ Vector.fromList list
  decode tt = case tt of
    TTypes.Type (Just vector) -> t where list = Vector.toList vector
                                         t    = typeFromListAt list 0
    TTypes.Type Nothing       -> Left "`types` field is missing"



instance Convert [Type] [TTypes.Type] where
  encode t  = map (encode) t
  decode tt = convert $ map (decode) tt
 