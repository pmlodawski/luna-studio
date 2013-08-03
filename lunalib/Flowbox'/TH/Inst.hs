{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances,
             TemplateHaskell #-}

module Flowbox'.TH.Inst (
  mkInst,
  mkInstIO
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

mkInstIO :: Name -> Name -> Name -> Name -> DecsQ    
mkInstIO tcName fromFun fromIOFun toFun =
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
