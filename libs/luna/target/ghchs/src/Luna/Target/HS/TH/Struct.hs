---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Luna.Target.HS.TH.Struct where

import           Control.Applicative
import           Control.Lens
import           Data.Maybe
import           Language.Haskell.TH
import qualified Luna.Target.HS.Host.Naming2 as Naming
import           Luna.Target.HS.TH.Utils
import           Luna.Target.HS.AST.Deriving (stdDerivings)
import           Data.Maybe                  (mapMaybe)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Monoid

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


--generateFieldAccessors :: Name -> [Maybe String] -> Q [Dec]
--generateFieldAccessors typeName fieldNames = do
--    (++) <$> generateFieldGetters typeName fieldNames
--         <*> generateFieldSetters typeName fieldNames

--generateFieldGetters conName fieldNames = do
--    typeName <- getTypeNameQ conName
--    unit     <- unitName
--    let argnum          = length fieldNames
--        namedPatterns   = fmap (over _1 fromJust) $ filter (isJust . view _1) $ (zip fieldNames patternsSet)
--        patternsSet     = genTNameSet VarP (VarP unit) (argnum - 1)
--        genPropAcc (fieldName, patterns) = getter where
--            getter  = FunD accName [Clause [ConP conName patterns] (NormalB $ VarE unit) []]
--            sig     = mkSimpleMemSig0 typeName fieldName
--            def     = mkSimpleMemDef 1 typeName fieldName accName
--            accName = mkName $ Naming.mkFieldGetter typeName fieldName
--    return $ fmap genPropAcc namedPatterns

--generateFieldSetters conName fieldNames = do
--    typeName <- getTypeNameQ conName
--    unit     <- unitName
--    let argnum        = length fieldNames
--        namedPatterns = fmap (over _1 fromJust) $ filter (isJust . view _1) $ (zip3 fieldNames patternsSet exprSet)
--        patternsSet   = genTNameSet VarP WildP (argnum - 1)
--        exprSet       = genTNameSet VarE (VarE unit) (argnum - 1)
--        genPropAcc (fieldName, patterns, appExprs) = [setter, sig, def] where
--            setter  = FunD accName [Clause [ConP conName patterns, VarP unit] (NormalB appExpr) []]
--            sig     = mkSimpleMemSig1 typeName setterName
--            def     = mkSimpleMemDef 2 typeName setterName accName
--            accName = mkName $ Naming.mkFieldSetter typeName fieldName
--            appExpr = foldl AppE (ConE conName) appExprs
--            setterName = mkName $ Naming.setter fieldName
--    return . concat $ fmap genPropAcc namedPatterns

appEs :: Exp -> [Exp] -> Exp
appEs = foldl AppE


--generateFieldAccessors (nameBase -> typeName) fieldDescs = return $ accessors ++ sigs 
--                                                                              ++ getterDefs 
--                                                                              ++ setterDefs 
--                                                                              -- ++ fncDefs 
--    where
--    unit = mkName "x"
--    obj  = mkName "obj"

--    rezipPatGens conName (fieldName, listGen) = (fieldName, [(conName, listGen)])
--    prepareGetters (conName, fieldNames) = fmap (rezipPatGens conName) $ mkArgPatGens fieldNames
--    mkPatMap   = Map.fromList . prepareGetters

--    consPatMap :: Map String [(Name, PatCons)] -- Map FieldName [(ConsName, PatCons)]
--    consPatMap = Map.unionsWith (++)
--               $ map mkPatMap fieldDescs
--    fieldNames  = Map.keys consPatMap
--    setterNames = fmap Naming.setter fieldNames

--    --accessors
--    mkGetter :: String -> [(Name, PatCons)] -> Dec
--    mkGetter fieldName descs = FunD accName [Clause [VarP obj] (NormalB $ CaseE (VarE obj) cases) []] where
--        accName  = mkName $ Naming.mkFieldGetter typeName fieldName
--        cases    = fmap (uncurry $ mkCase fieldName) descs
--        mkCase fieldName = mkAccCase (VarE unit) unit

--    mkSetter :: String -> [(Name, PatCons)] -> Dec
--    mkSetter fieldName descs = FunD accName [Clause [VarP obj, VarP unit] (NormalB $ CaseE (VarE obj) cases) []] where
--        accName  = mkName $ Naming.mkFieldSetter typeName fieldName
--        cases    = fmap (uncurry $ mkCase fieldName) descs
--        mkCase fieldName conName fieldGen = mkAccCase cons (mkName "_") conName fieldGen where
--            cons = appEs (ConE conName) (fmap VarE $ runPatCons fieldGen unit)

--    mkAccessor fieldName descs = [mkGetter fieldName descs, mkSetter fieldName descs]

--    mkAccCase result fieldName conName fieldGen = Match (ConP conName conPats) (NormalB result) [] where
--        conPats = fmap VarP $ runPatCons fieldGen fieldName

--    accessors = concat $ fmap (uncurry mkAccessor) $ Map.assocs consPatMap

--    -- sigs
--    getterSigs = fmap (mkSimpleMemSig0 typeName) fieldNames
--    setterSigs = fmap (mkSimpleMemSig1 typeName) setterNames
--    sigs       = getterSigs ++ setterSigs

--    --defs
--    getterDefs = fmap (mkGetterDef typeName) fieldNames
--    setterDefs = fmap (mkSetterDef typeName) fieldNames
--    --fncDefs    = fmap (mkFncDef typeName) fieldNames

--    mkGetterDef typeName fieldName = mkSimpleMemDef 1 typeName fieldName accName where
--        accName    = mkName $ Naming.mkFieldGetter typeName fieldName

--    mkSetterDef typeName fieldName = mkSimpleMemDef 2 typeName setterName accName where
--        setterName = Naming.setter fieldName
--        accName    = mkName $ Naming.mkFieldSetter typeName fieldName

genDataTuple tpName = do
    TyConI (DataD _ _ _ cons _) <- reify tpName
    mapM consTuple cons

    where consTuple c = do
              let name = getConName c
              vars <- mapM (\i -> newName $ "t" ++ show i) [1..consParams c]
              return $ FunD (mkName $ "dataTuple_" ++ nameBase tpName) 
                     $ [Clause [ConP name $ fmap VarP vars] (NormalB (TupE $ fmap VarE vars)) []]
          consParams = \case
              NormalC _ l   -> length l
              RecC    _ l   -> length l
              InfixC  {}    -> 2
              ForallC l _ _ -> length l


--genDataTuple = [FunD (mkName $ "mkgenDataTuple") [Clause [ConP Ghci1.Vector [VarP t1_1,VarP t2_2,VarP t3_3]] (NormalB (TupE [VarE t1_1,VarE t2_2,VarE t3_3])) []]]


{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

generateFieldAccessors (nameBase -> typeName) fieldDescs = return $ accessors ++ sigs 
                                                                              ++ getterDefs 
                                                                              ++ setterDefs 
                                                                              ++ getterFncs
                                                                              ++ setterFncs
                                                                              -- ++ fncDefs 
    where
    unit = mkName "x"
    obj  = mkName "obj"

    rezipPatGens conName (fieldName, listGen) = (fieldName, [(conName, listGen)])
    prepareGetters (conName, fieldNames) = fmap (rezipPatGens conName) $ mkArgPatGens fieldNames
    mkPatMap   = Map.fromList . prepareGetters

    consPatMap :: Map String [(Name, PatCons)] -- Map FieldName [(ConsName, PatCons)]
    consPatMap = Map.unionsWith (++)
               $ map mkPatMap fieldDescs
    fieldNames  = Map.keys consPatMap
    setterNames = fmap Naming.setter fieldNames

    --accessors
    mkGetter :: String -> [(Name, PatCons)] -> Dec
    mkGetter fieldName descs = FunD accName [Clause [VarP obj] (NormalB $ CaseE (VarE obj) cases) []] where
        accName  = mkName $ Naming.mkFieldGetter typeName fieldName
        cases    = fmap (uncurry mkCase) descs
        mkCase   = mkAccCase (AppE (VarE $ mkName "expandEl") $ VarE unit) unit

    mkSetter :: String -> [(Name, PatCons)] -> Dec
    mkSetter fieldName descs = FunD accName [Clause [VarP obj, VarP unit] (NormalB $ CaseE (VarE obj) cases) []] where
        accName  = mkName $ Naming.mkFieldSetter typeName fieldName
        cases    = fmap (uncurry mkCase) descs
        mkCase conName fieldGen = mkAccCase cons (mkName "_") conName fieldGen where
            cons       = appEs (ConE conName) 
                       $ fmap VarE $ runPatCons fieldGen unit

    mkAccessor fieldName descs = [mkGetter fieldName descs, mkSetter fieldName descs]

    mkAccCase result fieldName conName fieldGen = Match (ConP conName conPats) (NormalB result) [] where
        conPats = fmap VarP $ runPatCons fieldGen fieldName

    accessors = concat $ fmap (uncurry mkAccessor) $ Map.assocs consPatMap

    -- sigs
    getterSigNames = fmap (mkName . Naming.mkMemSig typeName) fieldNames
    getterDefNames = fmap (mkName . Naming.mkMemDef typeName) fieldNames
    getterFncNames = fmap (mkName . Naming.mkMemFnc typeName) fieldNames

    setterFieldNames = fmap Naming.setter fieldNames
    setterSigNames = fmap (mkName . Naming.mkMemSig typeName) setterFieldNames
    setterDefNames = fmap (mkName . Naming.mkMemDef typeName) setterFieldNames
    setterFncNames = fmap (mkName . Naming.mkMemFnc typeName) setterFieldNames

    mkMemFnc fname sigName defName = FunD fname [Clause [] (NormalB (TupE [VarE sigName, VarE defName])) []] where

    getterFncs = fmap (uncurry3 $ mkMemFnc) $ zip3 getterFncNames getterSigNames getterDefNames
    setterFncs = fmap (uncurry3 $ mkMemFnc) $ zip3 setterFncNames setterSigNames setterDefNames
    --mem

    getterSigs = fmap (mkSimpleMemSig0 typeName) fieldNames
    setterSigs = fmap (mkSimpleMemSig1 typeName) setterNames
    sigs       = getterSigs ++ setterSigs

    --defs
    getterDefs = fmap (mkGetterDef typeName) fieldNames
    setterDefs = fmap (mkSetterDef typeName) fieldNames
    --fncDefs    = fmap (mkFncDef typeName) fieldNames

    mkGetterDef typeName fieldName = mkSimpleMemDef typeName fieldName (mkLiftF 1 accName) where
        accName    = mkName $ Naming.mkFieldGetter typeName fieldName

    mkSetterDef typeName fieldName = mkSimpleMemDef typeName setterName (mkLiftFlatF 2 accName) where
        setterName = Naming.setter fieldName
        accName    = mkName $ Naming.mkFieldSetter typeName fieldName


newtype PatCons = PatCons { runPatCons :: Name -> [Name] }
instance Show PatCons where
    show (PatCons f) = "PatCons a "++ show (f $ mkName "a")


registerFieldAccessors typeName names = (++) <$> regMethods names <*> regMethods setterNames where
    setterNames = fmap Naming.setter names
    regMethods n = concat <$> mapM (registerMethod typeName) n

-- | Generates pattern name lists based on named args
-- input:
--    [Just "x", Nothing, Just "z"]
-- output:
--    [ ( "x" , a -> [ a , "t1" , "t2" ] )
--    , ( "z" , a -> [ "t0" , "t1" , a ] )
--    ]
mkArgPatGens :: [Maybe a] -> [(a, PatCons)]
mkArgPatGens mnames = pats where
    len          = length mnames
    pats         = mapMaybe (uncurry consPat) $ zip mnames patGens
    patGens      = fmap (PatCons . patList) [0..len-1]
    patList  i n = wildList 0 i ++ [n] ++ wildList (i+1) (len-i-1)
    wildList s i = take i $ fmap (mkName.("t" ++).show) [s..]
    consPat mname patGen = case mname of
        Nothing   -> Nothing
        Just name -> Just $ (name, patGen)

--genTNameSet elmod el n = tvars where
--    tnames = fmap elmod $ genTNameList n
--    tvars  = fmap (insertAt tnames el) [0..n]


mkSimpleMemSig pNum typeName fieldName = FunD fname [Clause [] (NormalB (VarE sigName)) []] where
    fname   = mkName $ Naming.mkMemSig typeName fieldName
    sigName = mkName $ "simpleFSig" ++ show pNum

mkSimpleMemSig0 = mkSimpleMemSig 0
mkSimpleMemSig1 = mkSimpleMemSig 1

mkSimpleMemDef typeName fieldName body = FunD fname [Clause [] (NormalB body) []] where
    fname = mkName $ Naming.mkMemDef typeName fieldName


mkLiftF pNum base = AppE (VarE fname) (VarE base) where
    fname = mkName $ "liftF" ++ show pNum

mkLiftFlatF pNum base = AppE (VarE fname) (VarE base) where
    fname = mkName $ "liftFlatF" ++ show pNum


--registerMethodSignature typeName methodName (Naming.toName -> funcName) = do
--    funcT   <- getTypeQ funcName
--    dataDec <- getDec typeName
--    let
--        dataVars   = map VarT $ getDecVarNames dataDec
--        baseT      = ConT typeName
--        nt         = foldl AppT (ConT . mkName $ Naming.classHasProp) [LitT (StrTyLit methodName), baseT, funcT]
--        funcs      = [FunD (mkName Naming.funcPropSig) [Clause [WildP] (NormalB (VarE funcName)) []]]
--        inst       = InstanceD [] nt funcs

--    return [inst]


--registerMethod :: Name -> String -> Q [Dec]
--registerMethod typeName methodName = do
--    let typeNameBase = nameBase typeName
--        funcSig      = Naming.mkMemSig typeNameBase methodName
--        funcDef      = Naming.mkMemDef typeNameBase methodName
--    (++) <$> registerMethodSignature  typeName methodName funcSig
--         <*> registerMethodDefinition typeName methodName funcDef


--registerMethod :: Name -> String -> Q [Dec]
--registerMethod typeName methodName = do
--    let typeNameBase = nameBase typeName
--        funcSig      = Naming.mkMemSig typeNameBase methodName
--        funcDef      = Naming.mkMemDef typeNameBase methodName
--    registerMethodDefinition typeName methodName funcDef

--registerMethodDefinition typeName methodName (Naming.toName -> funcName) = do
--    funcT   <- getTypeQ funcName
--    dataDec <- getDec typeName
--    argsT      <- VarT <$> newName "args"
--    let
--        dataVars   = map VarT $ getDecVarNames dataDec
--        baseT      = ConT typeName
--        prectx     = getContext funcT
--        sig        = getSignature funcT
--        --c1         = equalT argsT sig
--        nt         = foldl AppT (ConT $ mkName "MemberProvider") [baseT, LitT (StrTyLit methodName), argsT, sig]
--        funcs      = [FunD (mkName "getMember") [Clause [WildP, WildP] (NormalB (VarE funcName)) []]] 
--        ctx        = fmap fixCtx_GHC_7_8 $ prectx
--        inst       = InstanceD ctx nt funcs
--    return $ [inst]



registerType :: Name -> Q [Dec]
registerType tpName = do
    TyConI dec <- reify tpName
    let bndrs  = getDecBinders dec
        treg   = registerType' tpName bndrs
        clsreg = registerType' clsName []
        clsdef = DataD [] clsName [] [NormalC clsName []] (fmap (mkName.show) stdDerivings)
    return $ clsdef : (treg ++ clsreg)
    where clsName = mkName $ Naming.mkCls (nameBase tpName)

registerCons :: Name -> [String] -> Q [Dec]
registerCons tpName selNames = do
    TyConI (DataD _ _ _ cons _) <- reify tpName
    let selCons    = filter (\c -> nameBase (getConName c) `elem` selNames) cons
        conMakers  = fmap (genConMaker tpName) (fmap mkName selNames)
        conLayouts = fmap (genConLayout tpName) selCons
    return $ conMakers ++ conLayouts

registerType' :: Name -> [TyVarBndr] -> [Dec]
registerType' tpName bndrs = [decl, tfam] where
    params     = fmap getTyVarBndrName bndrs
    normalName = mkName $ Naming.mkTypePtr (nameBase tpName)
    decl = DataD [] normalName [] [NormalC normalName []] []
    tfam = TySynInstD (mkName "ProxyType") (TySynEqn [foldl AppT (ConT tpName) (fmap VarT params)] (ConT normalName))

genConMaker :: Name -> Name -> Dec
genConMaker tpName name = ValD (VarP . mkName $ "cons_" <> nameStr) (NormalB expr) [] where
    nameStr = nameBase name
    expr = appEs (VarE $ mkName "member")
         [ proxyE $ LitT (StrTyLit nameStr)
         , valE . ConE . mkName $ Naming.mkCls (nameBase tpName)
         ]

genConLayout :: Name -> Con -> Dec
genConLayout tpName con = FunD fname [Clause [ConP conName (fmap VarP fields)] (NormalB (rTupE (fmap VarE fields))) []] where
    fieldNum = getConFieldNumber con
    fields   = fmap (mkName.("t"<>).show) [1..fieldNum]
    conName  = getConName con
    fname    = mkName . Naming.mkLayout $ nameBase conName

registerMethod :: Name -> String -> Q [Dec]
registerMethod typeName methodName = do
    let typeNameBase = nameBase typeName
        funcSig      = Naming.mkMemSig typeNameBase methodName
        funcDef      = Naming.mkMemDef typeNameBase methodName
        funcFnc      = Naming.mkMemFnc typeNameBase methodName
    registerMethodDefinition typeName methodName funcFnc

registerMethodDefinition typeName methodName (Naming.toName -> funcName) = do
    funcT   <- getTypeQ funcName
    dataDec <- getDec typeName
    argsT      <- VarT <$> newName "args"
    resultT    <- VarT <$> newName "result"
    let
        dataVars   = map VarT $ getDecVarNames dataDec
        baseT      = ConT (mkName $ Naming.mkTypePtr $ nameBase typeName)
        --baseT      = ConT (mkName $ nameBase typeName ++ "_T")

        prectx     = getContext funcT
        sig        = getSignature funcT
        resultC    = equalT resultT sig
        nt         = foldl AppT (ConT $ mkName "MemberProvider") [baseT, LitT (StrTyLit methodName), argsT, resultT]
        funcs      = [FunD (mkName "getMember") [Clause [WildP, WildP] (NormalB (VarE funcName)) []]] 
        ctx        = fmap fixCtx_GHC_7_8 $ prectx
        inst       = InstanceD (resultC:ctx) nt funcs
    return $ [inst]

--registerMethodDefinition typeName methodName (Naming.toName -> funcName) = do
--    funcT   <- getTypeQ funcName
--    dataDec <- getDec typeName
--    argsT      <- VarT <$> newName "args"
--    let
--        dataVars   = map VarT $ getDecVarNames dataDec
--        baseT      = ConT typeName
--        prectx     = getContext funcT
--        sig        = getSignature funcT
--        --c1         = equalT argsT sig
--        nt         = foldl AppT (ConT $ mkName "MemberProvider") [baseT, LitT (StrTyLit methodName), argsT, sig]
--        funcs      = [FunD (mkName "getMember") [Clause [WildP, WildP] (NormalB (VarE funcName)) []]] 
--        ctx        = fmap fixCtx_GHC_7_8 $ prectx
--        inst       = InstanceD ctx nt funcs
--    return $ [inst]

--registerMethodDefinition typeName methodName (Naming.toName -> funcName) = do
--    funcT   <- getTypeQ funcName
--    dataDec <- getDec typeName
--    argsT      <- VarT <$> newName "args"
--    outT       <- VarT <$> newName "out"
--    let
--        dataVars   = map VarT $ getDecVarNames dataDec
--        baseT      = ConT typeName
--        prectx     = getContext funcT
--        (src, ret) = splitSignature $ getSignature funcT
--        c1         = equalT argsT src
--        c2         = equalT outT ret
--        nt         = foldl AppT (ConT $ mkName Naming.classFunc) [baseT, LitT (StrTyLit methodName), argsT, outT]
--        funcs      = [FunD (mkName Naming.funcGetFunc) [Clause [WildP, WildP] (NormalB (VarE funcName)) []]] 
--        ctx        = fmap fixCtx_GHC_7_8 $ c1:c2:prectx
--        inst       = InstanceD ctx nt funcs
--    return $ [inst]

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



genTNameList = genNameList "t"
genNameList prefix n = fmap (\x -> mkName $ prefix ++ show x) [1..n]

insertAt lst val idx = take idx lst ++ val : drop idx lst

unitName = newName "x"

--genWildPatterns2 n x = fmap (flip replicate WildP) [n,n-1..0]

