---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}


module Luna.Target.HS.Host.Naming2 where

import           Flowbox.Prelude
import           Language.Haskell.TH


class IsName a where
    toName   :: a -> Name
    fromName :: Name -> a

instance IsName Name where
    toName   = id
    fromName = id

instance IsName String where
    toName   = mkName
    fromName = nameBase

instance ToString Name where
    toString = nameBase

mkFieldAccessor :: (Monoid a, IsString a) => a -> a -> a -> a
mkFieldAccessor accName typeName memName =
    "field" <> accName
            <> "_" <> typeName
            <> "_" <> memName

mkFieldGetter :: (Monoid a, IsString a) => a -> a -> a
mkFieldGetter = mkFieldAccessor "Getter"

mkFieldSetter :: (Monoid a, IsString a) => a -> a -> a
mkFieldSetter = mkFieldAccessor "Setter"

classHasProp :: IsString a => a
classHasProp = "HasMem"

funcPropSig :: IsString a => a
funcPropSig  = "memSig"

classFunc :: IsString a => a
classFunc   = "Func"

funcGetFunc :: IsString a => a
funcGetFunc = "getFunc"

mkMemRef :: (Monoid a, IsString a) => a -> a -> a -> a
mkMemRef base typeName methodName =
    "mem" <> base
          <> "_" <> typeName
          <> "_" <> methodName

mkMemSig :: (Monoid a, IsString a) => a -> a -> a
mkMemSig = mkMemRef "Sig"

mkMemDef :: (Monoid a, IsString a) => a -> a -> a
mkMemDef = mkMemRef "Def"

mkMemFnc :: (Monoid a, IsString a) => a -> a -> a
mkMemFnc = mkMemRef "Fnc"

mkModCons :: (Monoid a, IsString a) => a -> a
mkModCons = mkCons

mkCons :: (Monoid a, IsString a) => a -> a
mkCons = ("cons_" <>)

mkLayout :: (Monoid a, IsString a) => a -> a
mkLayout = ("layout_" <>)

mkModName :: (Monoid a, IsString a) => a -> a
mkModName = ("Module" <>)

member :: IsString a => a
member = "member"

self :: IsString a => a
self = "self"

mkCls :: (Monoid a, IsString a) => a -> a
mkCls = ("Cls_" <>)

mkTypePtr :: (Monoid a, IsString a) => a -> a
mkTypePtr = (<> "_T")

-- hack fixing double underscore
mkCls' :: (Monoid a, IsString a) => a -> a
mkCls' = ("Cls#" <>)

mkVar :: (Monoid a, IsString a) => a -> a
mkVar = ("_" <>)

setter :: (Monoid a, IsString a) => a -> a
setter = ("set_" <>)

mkLamName :: (Monoid a, IsString a) => a -> a
mkLamName = ("l_" <>)

mkHandlerFuncName :: (Monoid a, IsString a) => a -> a
mkHandlerFuncName = ("h_" <>)
