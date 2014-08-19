---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Target.HS.TH.Struct where

import Language.Haskell.TH
import Luna.Target.HS.TH.Utils
import Data.Maybe
import Control.Applicative
import Control.Lens
import qualified Luna.Target.HS.Host.Naming as Naming

con2TypeName conName = do 
    DataConI _ _ typeName _ <- reify conName
    return typeName

 
generateFieldGetters typeName fieldNames = do
    TyConI (DataD _ _ _ cons _) <- reify typeName
    return $ concat $ fmap genCon cons
    where genCon (NormalC conName stypes) = fmap genPropGetter namedPatterns where
              namedPatterns = fmap (over _1 fromJust) $ filter (isJust . view _1) $ (zip fieldNames patternsSet)
              patternsSet   = genTNameSet VarP unitP (length stypes - 1)
              genPropGetter (fieldName, patterns) = FunD accName [Clause [ConP conName patterns] (NormalB unitE) []] where
                accName = Naming.mkFieldGetter typeName conName fieldName


generateFieldSetters typeName fieldNames = do
    TyConI (DataD _ _ _ cons _) <- reify typeName
    return $ concat $ fmap genCon cons
    where genCon (NormalC conName stypes) = fmap genPropGetter namedPatterns where
              namedPatterns = fmap (over _1 fromJust) $ filter (isJust . view _1) $ (zip3 fieldNames patternsSet exprSet)
              patternsSet   = genTNameSet VarP WildP (length stypes - 1)
              exprSet       = genTNameSet VarE unitE (length stypes - 1)
              genPropGetter (fieldName, patterns, appExprs) = FunD accName [Clause [unitP, ConP conName patterns] (NormalB appExpr) []] where
                  accName = Naming.mkFieldSetter typeName conName fieldName
                  appExpr = foldl AppE (ConE conName) appExprs
                  

registerMethodSignature typeName methodName (Naming.toName -> funcName) = do
    funcT   <- getType $ funcName
    dataDec <- getDec typeName
    let 
        dataVars   = map VarT $ getDecVarNames dataDec
        baseT      = ConT typeName
        nt         = foldl AppT (ConT Naming.classHasProp) [LitT (StrTyLit methodName), baseT, funcT]
        funcs      = [FunD Naming.funcPropSig [Clause [WildP] (NormalB (VarE funcName)) []]] 
        inst       = InstanceD [] nt funcs

    return [inst]

registerMethod typeName methodName = do
    let funcSig = Naming.mkMemSig typeName methodName
    let funcDef = Naming.mkMemDef typeName methodName
    (++) <$> registerMethodSignature  typeName methodName funcSig
         <*> registerMethodDefinition typeName methodName funcDef


registerMethodDefinition typeName methodName (Naming.toName -> funcName) = do
    funcT   <- getType $ funcName
    dataDec <- getDec typeName
    let 
        argsT      = VarT $ mkName "args"
        outT       = VarT $ mkName "out"
        dataVars   = map VarT $ getDecVarNames dataDec
        baseT      = ConT typeName
        ctx        = getContext funcT
        (src, ret) = getSignature funcT
        c1         = equalT argsT src
        c2         = equalT outT ret
        nt         = foldl AppT (ConT Naming.classFunc) [baseT, LitT (StrTyLit methodName), argsT, outT]
        funcs      = [FunD Naming.funcGetFunc [Clause [WildP, WildP] (NormalB (VarE funcName)) []]] 
        inst       = InstanceD (c1:c2:ctx) nt funcs
    return [inst]

equalT = AppT . (AppT EqualityT)


genTNameSet elmod el n = tvars where 
    tnames = fmap elmod $ genTNameList n
    tvars  = fmap (insertAt tnames el) [0..n]

generateFieldAccessors typeName fieldNames = do
    (++) <$> generateFieldGetters typeName fieldNames 
         <*> generateFieldSetters typeName fieldNames

genTNameList = genNameList "t"
genNameList prefix n = fmap (\x -> mkName $ prefix ++ show x) [1..n]

insertAt lst val idx = take idx lst ++ val : drop idx lst

unitName = mkName "x"
unitP = VarP unitName
unitE = VarE unitName

--genWildPatterns2 n x = fmap (flip replicate WildP) [n,n-1..0]

