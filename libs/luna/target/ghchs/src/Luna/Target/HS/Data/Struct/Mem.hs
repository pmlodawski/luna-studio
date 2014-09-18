---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Memrietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}


!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Data.Struct.Mem where

import GHC.TypeLits
import Data.Typeable (Proxy(..))
import Luna.Target.HS.Control
import Luna.Target.HS.Data.Func.App
import Luna.Target.HS.Data.Func.Args

import Data.Typeable
import Type.BaseType

----------------------------------------------------------------------------------
-- Mem (proxy datatype)
----------------------------------------------------------------------------------

data Mem obj (name :: Symbol) = Mem (Proxy obj) (Proxy name) deriving (Typeable)

instance Show (Mem obj name) <= (Typeable obj, KnownSymbol name, Typeable name) where
    show = show . typeOf

----------------------------------------------------------------------------------
-- HasMem
----------------------------------------------------------------------------------

class HasMem (name :: Symbol) obj sig | name obj -> sig where
    memSig :: Mem obj name -> sig


objPtr :: m (s a) -> out <= (BaseType (Proxy a) out, out~Proxy b)
objPtr el = Proxy

memPtr :: Proxy name -> m (s a) -> Mem obj name <= BaseType (Proxy a) (Proxy obj)
memPtr name obj = Mem (objPtr obj) name

getMem' :: Proxy name -> m (s a) -> AppH (Mem obj name) args <= (HasMem name obj args, BaseType (Proxy a) (Proxy obj))     
getMem' name obj = appH ptr (memSig ptr) where
    ptr = memPtr name obj

member' :: Proxy name -> m (s a) -> AppH (Mem obj name) args <= (HasMem name obj args1, BaseType (Proxy a) (Proxy obj), AppArgByName "self" (m (s a)) args1 args)
member' name obj = appByName' (Proxy::Proxy "self") obj $ getMem' name $ obj


getMem :: Proxy name -> m (s a) -> Value Pure (Safe (AppH (Mem obj name) args)) <= (HasMem name obj args, BaseType (Proxy a) (Proxy obj))     
getMem name obj = val $ appH ptr (memSig ptr) where
    ptr = memPtr name obj

member :: Proxy name -> m (s a) -> Value Pure (Safe (AppH (Mem obj name) args)) <= (HasMem name obj args1, BaseType (Proxy a) (Proxy obj), AppArgByName "self" (m (s a)) args1 args)
member name obj = appByName (Proxy::Proxy "self") obj $ getMem name $ obj

---

objPtr2 :: m base s a -> out <= (BaseType (Proxy a) out, out~Proxy b)
objPtr2 el = Proxy

memPtr2 :: Proxy name -> m base s a -> Mem obj name <= BaseType (Proxy a) (Proxy obj)
memPtr2 name obj = Mem (objPtr2 obj) name

--getMem2 :: Proxy name -> m (s a) -> Value Pure (Safe (AppH (Mem obj name) args)) <= (HasMem name obj args, BaseType (Proxy a) (Proxy obj))     
getMem2 name obj = valS $ appH ptr (memSig ptr) where
    ptr = memPtr2 name obj

member2 :: Proxy (name :: Symbol) -> m base s a -> ValueS Pure Safe (AppH (Mem obj name) args) <= (HasMem name obj args1, AppArgByName "self" (m base s a) args1 args, Type.BaseType.BaseType (Proxy a) (Proxy obj))
member2 name obj = appByName2 (Proxy::Proxy "self") obj $ getMem2 name $ obj

