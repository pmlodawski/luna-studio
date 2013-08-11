{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances,
             TemplateHaskell #-}

module Flowbox.Luna.Helpers.TH.Inst (
  mkInst,
  mkInst'',
  mkGetter,
  mkSetter,
  mkName
)
  where

import Control.Monad
import Language.Haskell.TH

pprint_me :: Ppr a => Q a -> Q String
pprint_me = liftM pprint

getType :: Name -> Q Type 
getType name =
  do
    reified <- reify name
    case reified of
      ClassOpI _ tp _ _ -> return tp
      DataConI _ tp _ _ -> return tp
      VarI     _ tp _ _ -> return tp
      TyVarI   _ tp     -> return tp
      _                 -> error "No type, sorry."

sourceRetTypes :: Q Type -> Q (Type, Type)
sourceRetTypes qtyp = 
  do
    typ <- qtyp
    case typ of
      ForallT _ _ tp             -> sourceRetTypes $ return tp
      AppT (AppT ArrowT src) ret -> return (src, ret)
      _                          -> error "It does not look like a function!"

mkInst :: Name -> Name -> Name -> DecsQ
mkInst tcName fromFun toFun =
  do
    (src, ret) <- sourceRetTypes $ getType fromFun
    let typ = return $ AppT (AppT (ConT tcName) src) ret
        cxt = 
         do
           typ <- getType fromFun
           case typ of     
             ForallT _ cxt _ -> return cxt
             _               -> return []
        funcs :: [Q Dec]
        funcs = [valD (varP toFun) (normalB (varE fromFun)) []]

    inst <- instanceD cxt typ funcs
    return [inst]

mkInst'' :: Name -> Name -> Name -> Name -> DecsQ    
mkInst'' tcName fromFun fromIOFun toFun =
  do
    (src, ret) <- sourceRetTypes $ getType fromFun
    let typ = return $ AppT (AppT (ConT tcName) src) ret
        cxt = 
         do
           typ <- getType fromFun
           case typ of     
             ForallT _ cxt _ -> return cxt
             _               -> return []
        toFunIO = mkName $ nameBase toFun ++ "\''M"
        funcs :: [Q Dec]
        funcs = [valD (varP toFun) (normalB (varE fromFun)) [],
                 valD (varP toFunIO) (normalB (varE fromIOFun)) []]

    inst <- instanceD cxt typ funcs
    return [inst]

isIO :: Name -> Q Bool
isIO name =
  do
    typ <- getType name
    (src, ret) <- sourceRetTypes $ return typ
    case ret of
      AppT (ConT cname) _ -> return $ show cname == "GHC.Types.IO"
      _                  -> return False
    

getCname :: Name -> Q Name
getCname name =
  do
    typ <- getType name
    (src, ret) <- sourceRetTypes $ return typ
    case ret of
      AppT (ConT cname) _ -> return cname
      _                  -> return $ mkName ""





appE2 x y z = AppE (AppE x y) z
appT2 x y z = AppT (AppT x y) z

mkGetter :: String -> Name -> DecsQ
mkGetter tcName' typeName = do
  let
    tcName          = mkName $ "C''" ++ tcName' ++"'getter'" 
    getterFunName   = mkName $ tcName' ++ "'getter'"
    primitiveGetter = mkName $ tcName' ++ "'F"
  
  (src, ret) <- sourceRetTypes $ getType getterFunName
  let
      typ1st = AppT (AppT (TupleT 2) (AppT (ConT typeName) (VarT $ mkName "a")))
                    (TupleT 0)
      typ2nd = AppT (AppT (TupleT 2) (VarT $ mkName "a"))
                    (TupleT 0)
      typ = return $
        ForallT [PlainTV $ mkName "a"] [] $
          AppT (AppT (ConT tcName) typ1st) typ2nd
      cxt :: Q Cxt
      cxt = return []
      toFunM = mkName $ nameBase getterFunName ++ "\''M"
      funDec1 :: Q Dec
      funDec1 = do
        patt <- varP $ mkName "v"
        let b1st = AppE (VarE $ mkName "select0'") (VarE $ mkName "v")
        let body = TupE [AppE (VarE primitiveGetter) b1st, TupE []]
        return $ FunD getterFunName [
          Clause [patt] (NormalB body) []
          ]
      funDec2 :: Q Dec
      funDec2 = do
        retvar <- varE $ mkName "return"
        dotvar <- varE $ mkName "."
        funvar <- varE getterFunName
        valD (varP toFunM)
          (normalB $ return $ appE2 dotvar retvar funvar) []
      funcs :: [Q Dec]
      funcs = [funDec1, funDec2]

  inst <- instanceD cxt typ funcs
  return [inst]



mkSetter :: String -> Name -> DecsQ
mkSetter tcName' typeName = do
  let
    tcName          = mkName $ "C''" ++ tcName' ++"'setter'" 
    getterFunName   = mkName $ tcName' ++ "'setter'"
    primitiveGetter = mkName $ tcName' ++ "'F"
  (src, ret) <- sourceRetTypes $ getType getterFunName
  let
      typ1st = AppT (AppT (TupleT 2) (AppT (ConT typeName) (VarT $ mkName "a")))
                    (appT2 (TupleT 2) (VarT $ mkName "a") (TupleT 0) )
      typ2nd = AppT (AppT (TupleT 2) (AppT (ConT typeName) (VarT $ mkName "a")))
                    (TupleT 0)
      typ = return $
        ForallT [PlainTV $ mkName "a"] [] $
          AppT (AppT (ConT tcName) typ1st) typ2nd
      cxt :: Q Cxt
      cxt = return []
      toFunM = mkName $ nameBase getterFunName ++ "\''M"
      funDec1 :: Q Dec
      funDec1 = do
        let patt = TupP[VarP $ mkName "k", TupP [VarP $ mkName "v", TupP []]]
        let fexp = [(primitiveGetter, VarE $ mkName "v")]
        let body = TupE [RecUpdE (VarE $ mkName "k") fexp, TupE []]
        return $ FunD getterFunName [
          Clause [patt] (NormalB body) []
          ]
      funDec2 :: Q Dec
      funDec2 = do
        retvar <- varE $ mkName "return"
        dotvar <- varE $ mkName "."
        funvar <- varE getterFunName
        valD (varP toFunM)
          (normalB $ return $ appE2 dotvar retvar funvar) []
      funcs :: [Q Dec]
      funcs = [funDec1, funDec2]

  inst <- instanceD cxt typ funcs
  return [inst]
