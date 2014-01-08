---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ViewPatterns           #-}


module FlowboxM.Luna.TH.Inst where

import           Control.Applicative
import           Control.Monad
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import qualified Text.Show.Pretty        as PP

ppTrace  x   = trace ("\n\n----------\n" ++ PP.ppShow x)
ppTraces s x = trace ("\n\n--- " ++ s ++ " ---\n" ++ PP.ppShow x)


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


getTyVarBndrName t = case t of
    PlainTV name    -> name
    KindedTV name _ -> name


pprint_me :: Ppr a => Q a -> Q String
pprint_me = liftM pprint

getType :: Name -> Q Type
getType name = do
    reified <- reify name
    return $ case reified of
        ClassOpI _ tp _ _ -> tp
        DataConI _ tp _ _ -> tp
        VarI     _ tp _ _ -> tp
        TyVarI   _ tp     -> tp
        _                 -> error "No type, sorry."


getRecNames :: Name -> Q [(Name, [Name])]
getRecNames name = do
    dec <- getDec name
    let DataD _ _ _ cons _ = dec
        conNames           = map (\(RecC name _) -> name) cons
        fieldNames         = map getConFieldNames cons
    return $ zip conNames fieldNames


getConFieldNames :: Con -> [Name]
getConFieldNames con = case con of
    RecC _ vars -> fmap (\(name,_,_) -> name) vars
    _           -> [] --error "Cannot get names from not RecC constructor."


getDec :: Name -> Q Dec
getDec name = do
    reified <- reify name
    return $ case reified of
        ClassI dec _  -> dec
        TyConI dec    -> dec
        FamilyI dec _ -> dec
        _                 -> error "No dec found."

getSignature :: Type -> (Type, Type)
getSignature t = case t of
                 ForallT _ _ tp             -> getSignature tp
                 AppT (AppT ArrowT src) ret -> (src, ret)
                 _                          -> error "It does not look like a function!"

splitSignature :: Type -> (Type, Type)
splitSignature t = case t of
                 ForallT _ _ tp             -> getSignature tp
                 AppT (AppT ArrowT src) ret -> (src, ret)
                 _                          -> error "It does not look like a function!"

getContext :: Type -> [Pred]
getContext t = case t of
               ForallT _ c _ -> c
               _             -> []

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

mkCallInsts :: Name -> Int -> Int -> DecsQ
mkCallInsts name argNum defNum = do
    let hashName   = "Handler_" ++ nameBase name
        callNames' = map (\x -> "call_" ++ (nameBase name) ++ "_" ++ show x) [argNum - defNum .. argNum]
        callNames  = map mkName callNames'
        hname      = mkName hashName

    concat <$> (sequence $ map (mkCallInst hname hname) callNames)

-- example: mkCallInst 'Handler_Vector 'Handler_Vector 'call_Vector_3
mkCallInst :: Name -> Name -> Name -> DecsQ
mkCallInst dataName conName funcName = do
    t <- getType funcName
    let tcName     = mkName "Call"
        toFun      = mkName "callProto"
        (src, ret) = splitSignature t
        (src2, ret2) = splitSignature ret
        cxt        = ctxBase : getContext t
        nt         = foldl AppT (ConT tcName) [AppT (ConT dataName) hBase, src2, ret2]
        ctxBase    = EqualP hBase src
        hBase      = VarT $ mkName "base"
        hVal       = mkName "val"
        funcs      = [funD toFun [clause [conP conName [varP hVal]] (normalB (  appE (varE funcName) (varE hVal)  )) []]] :: [Q Dec]

    inst   <- instanceD (pure cxt) (pure nt) funcs
    return [inst]


mkInstMem :: String -> Name -> DecsQ
mkInstMem mName funcSrc = do
    t <- getType funcSrc
    let (src, ret) = getSignature t
        cxt        = getContext t
        tcName2    = mkName "Member"
        funcDst2   = mkName "member"
        nt         = foldl AppT (ConT tcName2) [LitT (StrTyLit mName), src, ret]
        funcs      = [funD funcDst2 [clause [wildP] (normalB (conE funcSrc)) []]] :: [Q Dec]

    inst   <- instanceD (pure cxt) (pure nt) funcs
    return  $ [inst]

mkInstMem2 :: String -> Name -> Name -> Name -> DecsQ
mkInstMem2 mName fname fDat fCon = do
    --t <- getType fDat
    ft <- getType fname
    let --(src, ret)   = getSignature t
        (fsrc, fret) = getSignature ft
        ctx          = eqCtx : getContext ft
        eqCtx        = EqualP fhandler
                     $ AppT (ConT $ mkName "Pure")
                     $ AppT (ConT $ mkName "Safe")
                     $ AppT (ConT fDat) fret
        tcName2      = mkName "Member"
        funcDst2     = mkName "member"
        fhandler     = VarT $ mkName "fhandler"
        nt           = foldl AppT (ConT tcName2) [LitT (StrTyLit mName), fsrc, fhandler]
        handlerE     = infixE (Just $ conE fCon) dotE (Just $ varE fname)
        handlerValE  = infixE (Just $ varE $ mkName "val") dotE (Just handlerE)
        funcs        = [funD funcDst2 [clause [wildP] (normalB ( handlerValE )) []]] :: [Q Dec]

    inst   <- instanceD (pure ctx) (pure nt) funcs
    return [inst]



registerFunction :: Name -> Name -> String -> Int -> [ExpQ] -> DecsQ
registerFunction cname fname methodName argnum defaultsQ = do
    let hashName' = "Handler_" ++ nameBase fname
        hashName  = mkName $ hashName'
    fHandler <- mkFuncHandler hashName'
    inst     <- mkInstMem2 methodName fname hashName hashName
    defFuncs <- genDefArgFuncs (nameBase fname) argnum defaultsQ
    return $ fHandler ++ inst ++ defFuncs


registerClass :: Name -> Int -> [ExpQ] -> DecsQ
registerClass dataName argNum defaultsQ = do
    let conName' = "Handler_" ++ nameBase dataName
        conName  = mkName conName'
    handler     <- mkFuncHandler conName'
    liftedCon   <- liftCon dataName argNum
    defArgFuncs <- genDefArgFuncs (nameBase dataName) argNum defaultsQ
    return $ handler ++ liftedCon ++ defArgFuncs


registerLambda :: Name -> Int -> [ExpQ] -> DecsQ
registerLambda name argNum defaultsQ = do
    let conName' = "Handler_" ++ nameBase name
        conName  = mkName conName'
    handler     <- mkFuncHandler conName'
    liftedCon   <- liftConLambda name argNum
    defArgFuncs <- genDefArgFuncs (nameBase name) argNum defaultsQ
    return $ handler ++ liftedCon ++ defArgFuncs


genDefArgFuncs :: String -> Int -> [ExpQ] -> DecsQ
genDefArgFuncs nameSuffix num valsQ = do
    vals <- sequence valsQ
    let funcs        = map genFunc funcCons
        funcCons     = zip3 names tuples (reverse argMix)
        argMix       = mixes barNamesE vals
        tuples       = map genTuple (map (flip take barNamesP) [(num - length vals)..num])
        names        = map (\x -> mkName $ "call_" ++ nameSuffix ++ "_" ++ show x) [(num - length vals)..num]
        barNamesP    = map VarP barNames
        barNamesE    = map VarE barNames
        barNames     = map (\x -> mkName $ "v_" ++ show x) [1..num]
        name         = mkName $ "call_" ++ nameSuffix ++ "_"
        baseName     = mkName "base"

        genMix  a b i               = (take i (reverse b)) ++ drop i (reverse a)
        mixes   a b                 = map (reverse . (genMix a b)) [0..length b]
        genCallE ns                 = foldl AppE (VarE baseName) ns
        genFunc (name, tuple, args) = FunD name [Clause [VarP baseName, tuple] (NormalB $ genCallE args) []]

    return funcs

genTuple elems = case elems of
    e:[] -> ConP (mkName "OneTuple") elems
    _    -> TupP elems


baseFieldName :: Name -> Name -> String
baseFieldName (nameBase -> clsName) (nameBase -> hashName) = drop (length namePrefix) hashName where
    namePrefix = "m_" ++ clsName ++ "_"


generateClsAccessors :: Name -> DecsQ
generateClsAccessors clsName = do
    getters <- generateClsGetters clsName
    setters <- generateClsSetters clsName
    return $ getters ++ setters

registerClsAccessors :: Name -> DecsQ
registerClsAccessors clsName = do
    getters <- registerClsGetters clsName
    setters <- registerClsSetters clsName
    return $ getters ++ setters

mkCallInstsAccessors :: Name -> DecsQ
mkCallInstsAccessors clsName = do
    getters <- mkCallInstsGetters clsName
    setters <- mkCallInstsSetters clsName
    return $ getters ++ setters

getClsFieldNames :: Name -> Q [Name]
getClsFieldNames clsName' = do
    nameList <- getRecNames clsName'
    let clsName    = nameBase clsName'
        fieldNames = concat $ map (\(_, fNames) -> fNames) nameList
    return fieldNames


generateClsGetters :: Name -> DecsQ
generateClsGetters clsName' = do
    fieldNames <- getClsFieldNames clsName'
    concat <$> mapM (generateClsGetter clsName') fieldNames

generateClsSetters :: Name -> DecsQ
generateClsSetters clsName' = do
    fieldNames <- getClsFieldNames clsName'
    concat <$> mapM (generateClsSetter clsName') fieldNames

registerClsGetters :: Name -> DecsQ
registerClsGetters clsName' = do
    fieldNames <- getClsFieldNames clsName'
    let clsName    = nameBase clsName'
        realFNames = map (baseFieldName clsName') fieldNames
        getFNames  = map (mkName . mkFGetName clsName) realFNames
    concat <$> mapM (\(fname, realname) -> registerFunction clsName' fname realname 0 []) (zip getFNames realFNames)

registerClsSetters :: Name -> DecsQ
registerClsSetters clsName' = do
    fieldNames <- getClsFieldNames clsName'
    let clsName    = nameBase clsName'
        realFNames = map (baseFieldName clsName') fieldNames
        setNames   = map ("set_"++) realFNames
        getFNames  = map (mkName . mkFSetName clsName) realFNames
    concat <$> mapM (\(fname, realname) -> registerFunction clsName' fname realname 1 []) (zip getFNames setNames)


mkCallInstsGetters :: Name -> DecsQ
mkCallInstsGetters clsName' = do
    fieldNames <- getClsFieldNames clsName'
    let clsName    = nameBase clsName'
        realFNames = map (baseFieldName clsName') fieldNames
        getFNames  = map (mkName . mkFGetName clsName) realFNames
    concat <$> mapM (\fname -> mkCallInsts fname 0 0) getFNames


mkCallInstsSetters :: Name -> DecsQ
mkCallInstsSetters clsName' = do
    fieldNames <- getClsFieldNames clsName'
    let clsName    = nameBase clsName'
        realFNames = map (baseFieldName clsName') fieldNames
        getFNames  = map (mkName . mkFSetName clsName) realFNames
    concat <$> mapM (\fname -> mkCallInsts fname 1 0) getFNames

mkFGetName :: String -> String -> String
mkFGetName clsName fName = "fget_" ++ clsName ++ "_" ++ fName

mkFSetName :: String -> String -> String
mkFSetName clsName fName = "fset_" ++ clsName ++ "_" ++ fName




generateClsGetter :: Name -> Name -> DecsQ
generateClsGetter clsName' fieldName' = do
    let clsName   = nameBase clsName'
        realFName = baseFieldName clsName' fieldName'
        funcName  = mkName $ mkFGetName clsName realFName
        fVar      = mkName "a"
        fFlatten  = mkName "flattenCtx"
        fLiftf    = mkName "liftf"
        getFunc   = FunD funcName [Clause [VarP fVar] (NormalB (AppE (VarE fFlatten) (AppE (AppE (VarE fLiftf) (VarE fieldName')) (VarE fVar)))) []]
    return [getFunc]

generateClsSetter :: Name -> Name -> DecsQ
generateClsSetter clsName' fieldName' = do
    let clsName   = nameBase clsName'
        realFName = baseFieldName clsName' fieldName'
        funcName  = mkName $ mkFSetName clsName realFName
        aVar      = mkName "a"
        xVar      = mkName "x"
        valVar    = mkName "val"
        fLiftf    = mkName "liftf"
        setFunc   = FunD funcName [Clause [VarP aVar,VarP valVar] (NormalB (AppE (AppE (VarE fLiftf) (LamE [VarP xVar] (RecUpdE (VarE xVar) [(fieldName',VarE valVar)]))) (VarE aVar))) []]
    return [setFunc]

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


mkFuncHandler :: String -> DecsQ
mkFuncHandler hashName' = do
    let fVar         = mkName "a"
        hashName     = mkName hashName'
        showD        = mkName "Show"
        fhType       = NewtypeD empty hashName [PlainTV fVar] (NormalC hashName [(NotStrict,VarT fVar)]) [showD]
    return [fhType]

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



--mkInst'' :: Name -> Name -> Name -> Name -> DecsQ
--mkInst'' tcName fromFun fromIOFun toFun =
--  do
--    (src, ret) <- getSignature $ getType fromFun
--    let typ = return $ AppT (AppT (ConT tcName) src) ret
--        cxt =
--         do
--           typ <- getType fromFun
--           case typ of
--             ForallT _ cxt _ -> return cxt
--             _               -> return []
--        toFunIO = mkName $ nameBase toFun ++ "\''M"
--        funcs :: [Q Dec]
--        funcs = [valD (varP toFun) (normalB (varE fromFun)) [],
--                 valD (varP toFunIO) (normalB (varE fromIOFun)) []]

--    inst <- instanceD cxt typ funcs
--    return [inst]

--isIO :: Name -> Q Bool
--isIO name =
--  do
--    typ <- getType name
--    (src, ret) <- getSignature $ return typ
--    case ret of
--      AppT (ConT cname) _ -> return $ show cname == "GHC.Types.IO"
--      _                  -> return False


--getCname :: Name -> Q Name
--getCname name =
--  do
--    typ <- getType name
--    (src, ret) <- getSignature $ return typ
--    case ret of
--      AppT (ConT cname) _ -> return cname
--      _                  -> return $ mkName ""





--appE2 x y z = AppE (AppE x y) z
--appT2 x y z = AppT (AppT x y) z

--mkGetter :: String -> Name -> DecsQ
--mkGetter tcName' typeName = do
--  let
--    tcName          = mkName $ "C''" ++ tcName' ++"'getter'"
--    getterFunName   = mkName $ tcName' ++ "'getter'"
--    primitiveGetter = mkName $ tcName' ++ "'F"

--  (src, ret) <- getSignature $ getType getterFunName
--  let
--      typ1st = AppT (AppT (TupleT 2) (AppT (ConT typeName) (VarT $ mkName "a")))
--                    (TupleT 0)
--      typ2nd = AppT (AppT (TupleT 2) (VarT $ mkName "a"))
--                    (TupleT 0)
--      typ = return $
--        ForallT [PlainTV $ mkName "a"] [] $
--          AppT (AppT (ConT tcName) typ1st) typ2nd
--      cxt :: Q Cxt
--      cxt = return []
--      toFunM = mkName $ nameBase getterFunName ++ "\''M"
--      funDec1 :: Q Dec
--      funDec1 = do
--        patt <- varP $ mkName "v"
--        let b1st = AppE (VarE $ mkName "select0'") (VarE $ mkName "v")
--        let body = TupE [AppE (VarE primitiveGetter) b1st, TupE []]
--        return $ FunD getterFunName [
--          Clause [patt] (NormalB body) []
--          ]
--      funDec2 :: Q Dec
--      funDec2 = do
--        retvar <- varE $ mkName "return"
--        dotvar <- varE $ mkName "."
--        funvar <- varE getterFunName
--        valD (varP toFunM)
--          (normalB $ return $ appE2 dotvar retvar funvar) []
--      funcs :: [Q Dec]
--      funcs = [funDec1, funDec2]

--  inst <- instanceD cxt typ funcs
--  return [inst]



--mkSetter :: String -> Name -> DecsQ
--mkSetter tcName' typeName = do
--  let
--    tcName          = mkName $ "C''" ++ tcName' ++"'setter'"
--    getterFunName   = mkName $ tcName' ++ "'setter'"
--    primitiveGetter = mkName $ tcName' ++ "'F"
--  (src, ret) <- getSignature $ getType getterFunName
--  let
--      typ1st = AppT (AppT (TupleT 2) (AppT (ConT typeName) (VarT $ mkName "a")))
--                    (appT2 (TupleT 2) (VarT $ mkName "a") (TupleT 0) )
--      typ2nd = AppT (AppT (TupleT 2) (AppT (ConT typeName) (VarT $ mkName "a")))
--                    (TupleT 0)
--      typ = return $
--        ForallT [PlainTV $ mkName "a"] [] $
--          AppT (AppT (ConT tcName) typ1st) typ2nd
--      cxt :: Q Cxt
--      cxt = return []
--      toFunM = mkName $ nameBase getterFunName ++ "\''M"
--      funDec1 :: Q Dec
--      funDec1 = do
--        let patt = TupP[VarP $ mkName "k", TupP [VarP $ mkName "v", TupP []]]
--        let fexp = [(primitiveGetter, VarE $ mkName "v")]
--        let body = TupE [RecUpdE (VarE $ mkName "k") fexp, TupE []]
--        return $ FunD getterFunName [
--          Clause [patt] (NormalB body) []
--          ]
--      funDec2 :: Q Dec
--      funDec2 = do
--        retvar <- varE $ mkName "return"
--        dotvar <- varE $ mkName "."
--        funvar <- varE getterFunName
--        valD (varP toFunM)
--          (normalB $ return $ appE2 dotvar retvar funvar) []
--      funcs :: [Q Dec]
--      funcs = [funDec1, funDec2]

--  inst <- instanceD cxt typ funcs
--  return [inst]
