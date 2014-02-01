---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ViewPatterns           #-}


module Luna.Target.HS.TH.Accessors where

import           Control.Applicative
import           Control.Monad
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import qualified Text.Show.Pretty           as PP
import qualified Luna.Target.HS.Naming      as Naming
import qualified Luna.Target.HS.TH.Deriving as Deriving
import           Luna.Target.HS.TH.Utils
import           Luna.Target.HS.TH.Inst
import           Data.List (nub)

generateAccessors :: Name -> DecsQ
generateAccessors clsName = do
    getters <- generateGetters clsName
    setters <- generateSetters clsName
    return $ getters ++ setters

registerAccessors :: Name -> DecsQ
registerAccessors clsName = do
    getters <- registerGetters clsName
    setters <- registerSetters clsName
    return $ getters ++ setters

mkInstsAccessors :: Name -> DecsQ
mkInstsAccessors clsName = do
    getters <- mkInstsGetters clsName
    setters <- mkInstsSetters clsName
    return $ getters ++ setters


getClsFieldNames :: Name -> Q [Name]
getClsFieldNames clsName' = do
    nameList <- getRecNames clsName'
    let clsName    = nameBase clsName'
        fieldNames = concat $ map (\(_, fNames) -> fNames) nameList
    return $ uniqueNames fieldNames 

uniqueNames :: [Name] -> [Name]
uniqueNames names = map mkName $ nub $ map nameBase names

generateGetters :: Name -> DecsQ
generateGetters clsName' = do
    fieldNames <- getClsFieldNames clsName'
    concat <$> mapM (generateClsGetter clsName') fieldNames

generateSetters :: Name -> DecsQ
generateSetters clsName' = do
    fieldNames <- getClsFieldNames clsName'
    concat <$> mapM (generateClsSetter clsName') fieldNames


registerGetters :: Name -> DecsQ
registerGetters clsName = do
    fieldNames <- getClsFieldNames clsName
    let realFNames = map (Naming.baseFieldName clsName) fieldNames
        getFNames  = map (Naming.mkMemGetName clsName) realFNames
    concat <$> mapM (\fname -> registerFunc fname 1 []) getFNames

registerSetters :: Name -> DecsQ
registerSetters clsName = do
    fieldNames <- getClsFieldNames clsName
    let realFNames = map (Naming.baseFieldName clsName) fieldNames
        getFNames  = map (Naming.mkMemSetName clsName) realFNames
    concat <$> mapM (\fname -> registerFunc fname 2 []) getFNames


mkInstsGetters :: Name -> DecsQ
mkInstsGetters clsName = do
    fieldNames <- getClsFieldNames clsName
    let realFNames = map (Naming.baseFieldName clsName) fieldNames
        getFNames  = map (Naming.mkMemGetName clsName) realFNames
        strNames   = map nameBase realFNames
    out1 <- concat <$> mapM (\fname -> mkCallInsts fname 1 0) getFNames
    out2 <- concat <$> mapM (\(fname, realname) -> mkMemInst realname clsName fname) (zip getFNames strNames)
    return $ out1 ++ out2



mkInstsSetters :: Name -> DecsQ
mkInstsSetters clsName = do
    fieldNames <- getClsFieldNames clsName
    let realFNames = map (Naming.baseFieldName clsName) fieldNames
        getFNames  = map (Naming.mkMemSetName clsName) realFNames
        setNames   = map (nameBase . Naming.mkSetName) realFNames
    out1 <- concat <$> mapM (\fname -> mkCallInsts fname 2 0) getFNames
    out2 <- concat <$> mapM (\(fname, realname) -> mkMemInst realname clsName fname) (zip getFNames setNames)
    return $ out1 ++ out2


generateClsGetter :: Name -> Name -> DecsQ
generateClsGetter clsName fieldName = do
    let realFName = Naming.baseFieldName clsName fieldName
        funcName  = Naming.mkMemGetName clsName realFName
        fVar      = mkName "a"
        fFlatten  = mkName "flattenCtx"
        fLiftf    = mkName "liftf1"
        getFunc   = FunD funcName [Clause [VarP fVar] (NormalB (AppE (VarE fFlatten) (AppE (AppE (VarE fLiftf) (VarE fieldName)) (VarE fVar)))) []]
    return $ [getFunc]

generateClsSetter :: Name -> Name -> DecsQ
generateClsSetter clsName fieldName = do
    let realFName = Naming.baseFieldName clsName fieldName
        funcName  = Naming.mkMemSetName clsName realFName
        aVar      = mkName "a"
        xVar      = mkName "x"
        valVar    = mkName "val"
        fLiftf    = mkName "liftf1"
        setFunc   = FunD funcName [Clause [VarP aVar,VarP valVar] (NormalB (AppE (AppE (VarE fLiftf) (LamE [VarP xVar] (RecUpdE (VarE xVar) [(fieldName,VarE valVar)]))) (VarE aVar))) []]
    return [setFunc]