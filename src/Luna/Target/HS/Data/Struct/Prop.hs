---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
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

module Luna.Target.HS.Data.Struct.Prop where

import GHC.TypeLits
import Data.Typeable (Proxy(..))
import Luna.Target.HS.Data.Func.App
import Luna.Target.HS.Data.Func.Args

import Data.Typeable
import Type.BaseType

----------------------------------------------------------------------------------
-- Prop (proxy datatype)
----------------------------------------------------------------------------------

data Prop obj (name :: Symbol) = Prop (Proxy obj) (Proxy name) deriving (Typeable)

instance Show (Prop obj name) <= (Typeable obj, KnownSymbol name) where
    show = show . typeOf

----------------------------------------------------------------------------------
-- HasProp
----------------------------------------------------------------------------------

class HasProp (name :: Symbol) obj sig | name obj -> sig where
    propSig :: Prop obj name -> sig


objPtr :: m (s a) -> out <= (BaseType (Proxy a) out, out~Proxy b)
objPtr el = Proxy

propPtr :: Proxy name -> m (s a) -> Prop obj name <= BaseType (Proxy a) (Proxy obj)
propPtr name obj = Prop (objPtr obj) name

getProp :: Proxy name -> m (s a) -> AppH (Prop obj name) args <= (HasProp name obj args, BaseType (Proxy a) (Proxy obj))     
getProp name obj = appH ptr (propSig ptr) where
    ptr = propPtr name obj

objProp :: Proxy name -> m (s a) -> AppH (Prop obj name) args <= (HasProp name obj args1, BaseType (Proxy a) (Proxy obj), AppArgByName "self" (m (s a)) args1 args)
objProp name obj = appByName (Proxy::Proxy "self") obj $ getProp name $ obj