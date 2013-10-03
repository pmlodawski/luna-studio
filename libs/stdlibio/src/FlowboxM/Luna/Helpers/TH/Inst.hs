{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances,
             TemplateHaskell #-}

module FlowboxM.Luna.Helpers.TH.Inst where

import           Control.Monad         
import           Language.Haskell.TH   
import           Control.Applicative   
import qualified Text.Show.Pretty    as PP
import           Debug.Trace           

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

getSignature :: Type -> (Type, Type)
getSignature t = case t of
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
