---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
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

 
--generateFieldGetters typeName fieldNames = do
--    TyConI (DataD _ _ _ cons _) <- reify typeName
--    return $ concat $ fmap genCon cons
--    where genCon (NormalC conName stypes) = fmap genPropGetter namedPatterns where
--              namedPatterns = fmap (over _1 fromJust) $ filter (isJust . view _1) $ (zip fieldNames patternsSet)
--              patternsSet   = genTNameSet VarP unitP (length stypes - 1)
--              genPropGetter (fieldName, patterns) = FunD accName [Clause [ConP conName patterns] (NormalB unitE) []] where
--                accName = Naming.mkFieldGetter typeName conName fieldName


--generateFieldSetters typeName fieldNames = do
--    TyConI (DataD _ _ _ cons _) <- reify typeName
--    return $ concat $ fmap genCon cons
--    where genCon (NormalC conName stypes) = fmap genPropGetter namedPatterns where
--              namedPatterns = fmap (over _1 fromJust) $ filter (isJust . view _1) $ (zip3 fieldNames patternsSet exprSet)
--              patternsSet   = genTNameSet VarP WildP (length stypes - 1)
--              exprSet       = genTNameSet VarE unitE (length stypes - 1)
--              genPropGetter (fieldName, patterns, appExprs) = FunD accName [Clause [unitP, ConP conName patterns] (NormalB appExpr) []] where
--                  accName = Naming.mkFieldSetter typeName conName fieldName
--                  appExpr = foldl AppE (ConE conName) appExprs

generateFieldGetters conName fieldNames = do
    typeName <- getTypeNameQ conName
    unit     <- unitName
    let argnum        = length fieldNames
        namedPatterns = fmap (over _1 fromJust) $ filter (isJust . view _1) $ (zip fieldNames patternsSet)
        patternsSet   = genTNameSet VarP (VarP unit) (argnum - 1)
        genPropAcc (fieldName, patterns) = FunD accName [Clause [ConP conName patterns] (NormalB $ VarE unit) []] where
            accName = Naming.mkFieldGetter typeName conName fieldName
    return $ fmap genPropAcc namedPatterns


generateFieldSetters conName fieldNames = do
    typeName <- getTypeNameQ conName
    unit     <- unitName
    let argnum        = length fieldNames
        namedPatterns = fmap (over _1 fromJust) $ filter (isJust . view _1) $ (zip3 fieldNames patternsSet exprSet)
        patternsSet   = genTNameSet VarP WildP (argnum - 1)
        exprSet       = genTNameSet VarE (VarE unit) (argnum - 1)
        genPropAcc (fieldName, patterns, appExprs) = FunD accName [Clause [VarP unit, ConP conName patterns] (NormalB appExpr) []] where
            accName = Naming.mkFieldSetter typeName conName fieldName
            appExpr = foldl AppE (ConE conName) appExprs
    return $ fmap genPropAcc namedPatterns
                  

registerMethodSignature typeName methodName (Naming.toName -> funcName) = do
    funcT   <- getTypeQ funcName
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
    funcT   <- getTypeQ funcName
    dataDec <- getDec typeName
    argsT      <- VarT <$> newName "args"
    outT       <- VarT <$> newName "out"
    let 
        dataVars   = map VarT $ getDecVarNames dataDec
        baseT      = ConT typeName
        prectx     = getContext funcT
        (src, ret) = splitSignature $ getSignature funcT
        c1         = equalT argsT src
        c2         = equalT outT ret
        nt         = foldl AppT (ConT Naming.classFunc) [baseT, LitT (StrTyLit methodName), argsT, outT]
        funcs      = [FunD Naming.funcGetFunc [Clause [WildP, WildP] (NormalB (VarE funcName)) []]] 
        ctx        = fmap fixCtx_GHC_7_8 $ c1:c2:prectx
        inst       = InstanceD ctx nt funcs
    return $ [inst]

-- GHC up to 7.9 has a bug when reifying contexts of classes compiled with -XPolyKinds
-- it adds GHC.Prim.* as first argument to each such constraint
fixCtx_GHC_7_8 ctx = case ctx of
    ClassP name t -> ClassP name t' where
        t' = filter checkStar t
        checkStar t = case t of
            ConT name -> if (nameBase name == "*") then False else True
            _         -> True
    _             -> ctx


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 709
equalT = AppT . (AppT EqualityT)
#else
equalT = EqualP
#endif



genTNameSet elmod el n = tvars where 
    tnames = fmap elmod $ genTNameList n
    tvars  = fmap (insertAt tnames el) [0..n]

generateFieldAccessors :: Name -> [Maybe String] -> Q [Dec]
generateFieldAccessors typeName fieldNames = do
    (++) <$> generateFieldGetters typeName fieldNames 
         <*> generateFieldSetters typeName fieldNames

genTNameList = genNameList "t"
genNameList prefix n = fmap (\x -> mkName $ prefix ++ show x) [1..n]

insertAt lst val idx = take idx lst ++ val : drop idx lst

unitName = newName "x"

--genWildPatterns2 n x = fmap (flip replicate WildP) [n,n-1..0]

