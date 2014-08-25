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


module Luna.Target.HS.TH.Utils where

import           Control.Applicative
import           Control.Monad
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Lib
import qualified Text.Show.Pretty    as PP

ppTrace  x   = trace ("\n\n----------\n" ++ PP.ppShow x)
ppTraces s x = trace ("\n\n--- " ++ s ++ " ---\n" ++ PP.ppShow x)


genNameBases num = map (("v_"++).show) [1..num]

genNames num = map mkName $ genNameBases num


getTyVarBndrName t = case t of
    PlainTV  name   -> name
    KindedTV name _ -> name


getDecVarNames :: Dec -> [Name]
getDecVarNames dec = map getTyVarBndrName vars where
    vars = case dec of 
        DataD    _ _ v _ _ -> v
        NewtypeD _ _ v _ _ -> v
        ClassD   _ _ v _ _ -> v
        FamilyD  _ _ v _   -> v
        TySynD   _ v _     -> v

getDecName :: Dec -> Name
getDecName dec = case dec of
    FunD         n _       -> n
    DataD        _ n _ _ _ -> n
    NewtypeD     _ n _ _ _ -> n
    TySynD       n _ _     -> n
    ClassD       _ n _ _ _ -> n
    SigD         n _       -> n 
    FamilyD      _ n _ _   -> n
    DataInstD    _ n _ _ _ -> n
    NewtypeInstD _ n _ _ _ -> n
    --TySynInstD   n _ _     -> n
    _                      -> error "This Dec does not have a name!"


pprint_me :: Ppr a => Q a -> Q String
pprint_me = liftM pprint

getType :: Info -> Type
getType info = case info of
        ClassOpI _ tp _ _ -> tp
        DataConI _ tp _ _ -> tp
        VarI     _ tp _ _ -> tp
        TyVarI   _ tp     -> tp
        _                 -> error "No type, sorry."


getTypeQ :: Name -> Q Type
getTypeQ name = getType <$> reify name


mkFuncAlias :: Name -> Name -> Dec
mkFuncAlias srcName dstName = ValD (VarP dstName) (NormalB (VarE srcName)) []


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


getRecNames :: Name -> Q [(Name, [Name])]
getRecNames name = do
    dec <- getDec name
    let DataD _ _ _ cons _ = dec
        conNames           = map (\(RecC name _) -> name) cons
        fieldNames         = map getConFieldNames cons
    return $ zip conNames fieldNames


--getSignature :: Type -> (Type, Type)
--getSignature t = case t of
--                 ForallT _ _ tp             -> getSignature tp
--                 AppT (AppT ArrowT src) ret -> (src, ret)
--                 _                          -> error "It does not look like a function!"

getSignature :: Type -> Type
getSignature t = case t of
                 ForallT _ _ tp             -> tp
                 AppT (AppT ArrowT src) ret -> AppT (AppT ArrowT src) ret
                 _                          -> error "It does not look like a function!"

splitSignature :: Type -> (Type, Type)
splitSignature t = case getSignature t of
                 AppT (AppT ArrowT src) ret -> (src, ret)
                 _                          -> error "It does not look like a signature!"


getArity :: Type -> Int
getArity t = case t of
    AppT (AppT ArrowT src) ret -> 1 + getArity ret
    _                          -> 0

getContext :: Type -> [Pred]
getContext t = case t of
               ForallT _ c _ -> c
               _             -> []



getTypeNameQ conName = getTypeName <$> reify conName


getTypeName (DataConI _ _ pname _) = pname
