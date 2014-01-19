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


module Luna.Target.HS.TH.Inst where

import           Control.Applicative
import           Control.Monad
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import qualified Text.Show.Pretty           as PP
import qualified Luna.Target.HS.Naming      as Naming
import qualified Luna.Target.HS.TH.Deriving as Deriving
import           Luna.Target.HS.TH.Utils

--import qualified Language.Haskell.Meta.Syntax.Translate as LH
--import qualified Language.Haskell.Exts.Syntax           as LH


mkSelfTypedName :: String -> String
mkSelfTypedName = (++ "_ST")

mkRTuple' :: [ExpQ] -> ExpQ
mkRTuple' a = do
    lst <- sequence a
    return $ mkRTupleE lst

mkRTupleE :: [Exp] -> Exp
mkRTupleE []     = TupE []
mkRTupleE (x:xs) = TupE [x, mkRTupleE xs]

mkRTupleP :: [Pat] -> Pat
mkRTupleP []     = TupP []
mkRTupleP (x:xs) = TupP [x, mkRTupleP xs]

mkNTWrapper ntname basename = do
    r <- reify basename
    let dec = case r of
              TyConI d -> d
              _        -> error "This function works only with data types!"

        (name, tyVarBndr) = case dec of
            DataD _ name' tyVarBndr' _ _ -> (name', tyVarBndr')
            TySynD  name' tyVarBndr' _   -> (name', tyVarBndr')
        tyNames = map (VarT . getTyVarBndrName) tyVarBndr
        ntTypeName = mkName ntname
        ntConName  = mkName ntname
        ntGetter   = mkName $ "get" ++ ntname
        pureCon    = ConT $ mkName "Pure"
        tbase      = AppT pureCon $ foldl AppT (ConT name) tyNames
        out = NewtypeD [] ntTypeName tyVarBndr (RecC ntConName [(ntGetter, NotStrict, tbase)]) []
    return $ [out]



mkInst :: Name -> Name -> Name -> DecsQ
mkInst tcName fromFun toFun = do
    t <- getType fromFun
    let (src, ret) = getSignature t
        cxt        = getContext t
        nt         = AppT (AppT (ConT tcName) src) ret
        funcs      = [valD (varP toFun) (normalB (varE fromFun)) []] :: [Q Dec]

    inst   <- instanceD (pure cxt) (pure nt) funcs
    return  $ [inst]


mkInst3 :: Name -> Name -> Name -> Name -> Name -> DecsQ
mkInst3 tcName fDat fCon fromFun toFun = do
    t <- getType fromFun
    let (src, ret) = getSignature t
        cxt        = ctxBase : getContext t
        nt         = foldl AppT (ConT tcName) [AppT (ConT fDat) hBase, ret]
        ctxBase    = EqualP hBase src
        hBase      = VarT $ mkName "base"
        hVal       = mkName "val"
        funcs      = [funD toFun [clause [conP fCon [varP hVal]] (normalB (  appE (varE fromFun) (varE hVal)  )) []]] :: [Q Dec]

    inst   <- instanceD (pure cxt) (pure nt) funcs
    return  $ [inst]


mkFuncCallInsts :: Name -> Int -> Int -> DecsQ
mkFuncCallInsts fName = mkCallInsts (mkName.mkSelfTypedName.nameBase $ fName)


--mkMemberCallInsts :: Name -> Int -> Int -> DecsQ
--mkMemberCallInsts dataName funcName argNum defNum = do
--    let hashName   = "Handler_" ++ nameBase name
--        callNames' = map (\x -> "call_" ++ (nameBase name) ++ "_" ++ show x) [argNum - defNum .. argNum]
--        callNames  = map mkName callNames'
--        hname      = mkName hashName

--    concat <$> (sequence $ map (mkCallInst hname hname) callNames)

--mkCallInst2 ''Handler_m_Vector_Vector 'm_Vector_Vector 'call_m_Vector_Vector_3 
--mkClsCallInsts 'Vector 3 0


--mkCallInsts :: Name -> Int -> Int -> DecsQ
--mkCallInsts name argNum defNum = do
--    let hashName   = "Handler_" ++ nameBase name
--        callNames' = map (\x -> "call_" ++ (nameBase name) ++ "_" ++ show x) [argNum - defNum .. argNum]
--        callNames  = map mkName callNames'
--        hname      = mkName hashName

--    concat <$> (sequence $ map (mkCallInst hname hname) callNames)

---- example: mkCallInst 'Handler_Vector 'Handler_Vector 'call_Vector_3
--mkCallInst :: Name -> Name -> Name -> DecsQ
--mkCallInst dataName conName funcName = do
--    t <- getType funcName
--    let tcName     = mkName "Call"
--        toFun      = mkName "callProto"
--        (src, ret) = splitSignature t
--        (src2, ret2) = splitSignature ret
--        cxt        = ctxBase : getContext t
--        nt         = foldl AppT (ConT tcName) [AppT (ConT dataName) hBase, src2, ret2]
--        ctxBase    = EqualP hBase src
--        hBase      = VarT $ mkName "base"
--        hVal       = mkName "val"
--        funcs      = [funD toFun [clause [conP conName [varP hVal]] (normalB (  appE (varE funcName) (varE hVal)  )) []]] :: [Q Dec]

--    inst   <- instanceD (pure cxt) (pure nt) funcs
--    return [inst]

-- mkCallInsts 'm_Vector_ftup 3 2
-- generates Call instances for 3 argument function, with 2 default arguments, using:
-- call_m_Vector_ftup_3, call_m_Vector_ftup_2 etc.
mkCallInsts :: Name -> Int -> Int -> DecsQ
mkCallInsts funcName argNum defNum = do
    let handlerName  = Naming.mkHandlerName funcName
        callNameBase = Naming.mkCallName    funcName
        callNames    = map (mkName.(\x -> nameBase callNameBase ++ "_" ++ show x)) [argNum - defNum .. argNum]
    concat <$> (sequence $ map (mkCallInst handlerName) callNames)

-- mkCallInst2 ''Handler_m_Vector_Vector 'call_m_Vector_Vector_3 
-- produces:
--     instance Call Handler_m_Vector_Vector (RTuple3 a a a) (Pure(Safe(Vector a))) where
--         callProto _ = call_m_Vector_Vector_3
mkCallInst :: Name -> Name -> DecsQ
mkCallInst dataName funcName = do
    t <- getType funcName
    let tcName     = mkName "Call"
        toFun      = mkName "callProto"
        (src, ret) = splitSignature t
        cxt        = getContext t
        nt         = foldl AppT (ConT tcName) [ConT dataName, src, ret]
        funcs      = [FunD toFun [Clause [WildP] (NormalB (VarE funcName)) []]]
        inst       = InstanceD cxt nt funcs
    return [inst]


mkMemInst :: String -> Name -> Name -> DecsQ
mkMemInst mName dataName funcName = do
    let funcHName = Naming.mkHandlerFuncName funcName
    funcT   <- getType funcHName
    dataDec <- getDec dataName
    let 
        dataVars   = map VarT $ getDecVarNames dataDec
        baseT      = AppT Naming.mVarT
                   $ AppT Naming.sVarT
                   $ foldl AppT (ConT dataName) dataVars
        nt         = foldl AppT (ConT Naming.memberClass) [LitT (StrTyLit mName), baseT, funcT]
        funcs      = [FunD Naming.memberClassFunc [Clause [WildP, WildP] (NormalB (VarE funcHName)) []]] 
        inst       = InstanceD [] nt funcs
    return  $ [inst]


--mkFuncType :: Name -> [LH.Type] -> DecsQ
--mkFuncType funcName typesLH = do
--    t <- getType funcName
--    let types = map LH.toType typesLH
--    return $ []

--mkInstMem2 :: String -> Name -> Name -> Name -> DecsQ
--mkInstMem2 mName fname fDat fCon = do
--    --t <- getType fDat
--    ft <- getType fname
--    let --(src, ret)   = getSignature t
--        (fsrc, fret) = getSignature ft
--        ctx          = eqCtx : getContext ft
--        eqCtx        = EqualP fhandler 
--                     $ AppT (ConT $ mkName "Pure") 
--                     $ AppT (ConT $ mkName "Safe") 
--                     $ AppT (ConT fDat) fret
--        tcName2      = mkName "Member"
--        funcDst2     = mkName "member"
--        fhandler     = VarT $ mkName "fhandler"
--        nt           = foldl AppT (ConT tcName2) [LitT (StrTyLit mName), fsrc, fhandler]
--        handlerE     = infixE (Just $ conE fCon) dotE (Just $ varE fname)
--        handlerValE  = infixE (Just $ varE $ mkName "val") dotE (Just handlerE)
--        funcs        = [funD funcDst2 [clause [wildP] (normalB ( handlerValE )) []]] :: [Q Dec]

--    inst   <- instanceD (pure ctx) (pure nt) funcs
--    return [inst]


mkSelfTyped :: Name -> Name -> DecsQ
mkSelfTyped fName dName = do
    TyConI dec <- reify dName
    let varNames = getDecVarNames dec
        varPats  = map VarT varNames
        dataName = getDecName dec
        selfName = mkName "self"
        mName    = mkName "m"
        sName    = mkName "s"
        outName  = mkName $ mkSelfTypedName $ nameBase fName
        dataPat  = foldr AppT (foldl AppT (ConT dataName) varPats) [VarT mName, VarT sName]
        selfPat  = SigP (VarP selfName) dataPat
        func     = FunD outName [Clause [selfPat] (NormalB (AppE (VarE fName) (VarE selfName))) []]
    return [func]


-- Self Typed function (self argument is typed) registerFunctionST
--registerFunctionST :: Name -> Name -> String -> Int -> [ExpQ] -> DecsQ
--registerFunctionST fName = registerFunction $ mkName $ mkSelfTypedName (nameBase fName)

--registerFunction :: Name -> Name -> String -> Int -> [ExpQ] -> DecsQ
--registerFunction fName cname methodName argnum defaultsQ = do
--    let hashName' = "Handler_" ++ nameBase fName
--        hashName  = mkName $ hashName'
--    fHandler <- mkFuncHandler hashName'
--    inst     <- mkInstMem2 methodName fName hashName hashName
--    defFuncs <- genDefArgFuncs (nameBase fName) argnum defaultsQ
--    return $ fHandler ++ inst ++ defFuncs


genCon a = do
    return []


registerCon :: Name -> Name -> Int -> [ExpQ] -> DecsQ
registerCon dataName conName argNum defaultsQ = do
    let handlerFuncName = Naming.mkHandlerFuncName memberName
        conFuncName     = Naming.mkConName conName
        memberName      = Naming.mkMemName dataName conName

        liftedCon   = liftFunc (ConE conName) memberName argNum
        conFunc     = mkFuncAlias handlerFuncName conFuncName

    funcReg <- registerFunc memberName argNum defaultsQ

    return $ liftedCon
           : conFunc
           : funcReg


registerFunc :: Name -> Int -> [ExpQ] -> DecsQ
registerFunc funcName argNum defaultsQ = do
    defaults <- sequence defaultsQ
    let handlerName     = Naming.mkHandlerName funcName
        handlerFuncName = Naming.mkHandlerFuncName funcName

        handler     = mkFuncHandler0 handlerName
        handlerFunc = liftFunc (ConE handlerName) handlerFuncName 0
        defArgFuncs = genDefArgFuncs funcName (VarE funcName) argNum defaults

    return $ handler
           : handlerFunc
           : defArgFuncs




--registerLambda :: Name -> Int -> [ExpQ] -> DecsQ
--registerLambda name argNum defaultsQ = do
--    let conName' = "Handler_" ++ nameBase name 
--        conName  = mkName conName'
--    handler     <- mkFuncHandler conName'
--    liftedCon   <- liftConLambda name argNum
--    defArgFuncs <- genDefArgFuncs (nameBase name) argNum defaultsQ
--    return $ handler ++ liftedCon ++ defArgFuncs



genDefArgFuncs :: Name -> Exp -> Int -> [Exp] -> [Dec]
genDefArgFuncs nameSuffix' func num vals = funcs where
    nameSuffix   = nameBase nameSuffix'
    funcs        = map genFunc funcCons
    funcCons     = zip3 names tuples (reverse argMix)
    argMix       = mixes barNamesE vals
    tuples       = map mkRTupleP (map (flip take barNamesP) [(num - length vals)..num])
    names        = map (\x -> mkName $ "call_" ++ nameSuffix ++ "_" ++ show x) [(num - length vals)..num]
    barNamesP    = map VarP barNames
    barNamesE    = map VarE barNames
    barNames     = map (\x -> mkName $ "v_" ++ show x) [1..num]
    name         = mkName $ "call_" ++ nameSuffix ++ "_"
    baseName     = mkName "Base"

    genMix  a b i               = (take i (reverse b)) ++ drop i (reverse a)
    mixes   a b                 = map (reverse . (genMix a b)) [0..length b]
    genCallE ns                 = foldl AppE func ns
    genFunc (name, tuple, args) = FunD name [Clause [tuple] (NormalB $ genCallE args) []]

genTuple elems = case elems of
    e:[] -> ConP (mkName "OneTuple") elems
    _    -> TupP elems






liftCon :: Name -> Int -> DecsQ
liftCon dataName argNum = do
    let conName  = mkName $ "Handler_" ++ nameBase dataName 
        funcName = mkName $ "con_" ++ nameBase dataName
        dotf     = mkName $ "dot" ++ show argNum
        liftf    = mkName "val"
        argsE    = AppE (AppE (VarE dotf) (VarE liftf)) (ConE dataName)
        handlerE = AppE (ConE conName) argsE
        valE     = AppE (VarE liftf) handlerE
        nCon     = ValD (VarP funcName) (NormalB valE) []
        --nCon     = ValD (VarP funcName) (NormalB (AppE (ConE conName) (ConE dataName))) []
    return [nCon]


liftCon' :: Name -> Int -> DecsQ
liftCon' dataName argNum = do
    let conName  = mkName $ "Handler_" ++ nameBase dataName 
        funcName = mkName $ "con_" ++ nameBase dataName
        liftf    = mkName "val"
        valE     = AppE (VarE liftf) $ ConE conName
        nCon     = ValD (VarP funcName) (NormalB valE) []
    return [nCon]


liftConLambda :: Name -> Int -> DecsQ
liftConLambda name argNum = do
    let conName  = mkName $ "Handler_" ++ nameBase name 
        funcName = mkName $ "con_" ++ nameBase name
        dotf     = mkName $ "dot" ++ show argNum
        liftf    = mkName "val"
        handlerE = AppE (ConE conName) (VarE name)
        valE     = AppE (VarE liftf) handlerE
        nCon     = ValD (VarP funcName) (NormalB valE) []
    return [nCon]


liftFunc :: Exp -> Name -> Int -> Dec
liftFunc srcExp dstFunc argNum = func where
    varNames = genNames argNum
    varsP    = map VarP varNames
    varsE    = map VarE varNames
    bodyBase = foldl AppE srcExp varsE
    bodyLift = AppE Naming.liftE bodyBase
    func     = FunD dstFunc [Clause varsP (NormalB bodyLift) []]

mkFuncHandler :: String -> DecsQ
mkFuncHandler hashName' = do
    let fVar         = mkName "a"
        hashName     = mkName hashName'
        showD        = mkName "Show"
        fhType       = NewtypeD empty hashName [PlainTV fVar] (NormalC hashName [(NotStrict,VarT fVar)]) [showD]
    return [fhType]

mkFuncHandler0 :: Name -> Dec
mkFuncHandler0 hashName = fhType where
    showD        = Naming.showName
    fhType       = DataD [] hashName [] [NormalC hashName []] [showD]

dotE = varE $ mkName "."



mkInstC :: Name -> Name -> Name -> DecsQ
mkInstC tcName fromFun toFun = do
    t <- getType fromFun
    let (src, ret) = getSignature t
        cxt        = getContext t
        nt         = AppT (AppT (ConT tcName) src) ret
        funcs      = [valD (varP toFun) (normalB (conE fromFun)) []] :: [Q Dec]

    inst   <- instanceD (pure cxt) (pure nt) funcs
    return  $ [inst]


mkInst2 :: Name -> Name -> Name -> Name -> Name -> DecsQ
mkInst2 tcName dtIdent fromFunRef fromFun toFun = do
    t <- getType fromFunRef
    let (src, ret) = getSignature t
        cxt        = getContext t
        nt         = AppT(
                         AppT(
                             AppT
                                 (ConT tcName)
                                 (ConT dtIdent)
                         )
                         src
                     )
                     ret
        funcs      = [valD (varP toFun) (normalB (varE fromFun)) []] :: [Q Dec]

    inst   <- instanceD (pure cxt) (pure nt) funcs
    return  $ [inst]

