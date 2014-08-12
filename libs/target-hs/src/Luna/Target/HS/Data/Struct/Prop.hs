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
import Luna.Target.HS.Data.Func

import Data.Typeable

----------------------------------------------------------------------------------
-- Prop (proxy datatype)
----------------------------------------------------------------------------------

data Prop obj (name :: Symbol) = Prop (Proxy obj) (Proxy name) deriving (Typeable)

instance Show (Prop obj name) <= (Typeable obj, KnownSymbol name) where
    show = show . typeOf

----------------------------------------------------------------------------------
-- HasProp
----------------------------------------------------------------------------------

class HasProp (name :: Symbol) obj fptr | name obj -> fptr where
    getProp :: Proxy name -> obj -> fptr


class HasProp2 (name :: Symbol) obj sig | name obj -> sig where
    propSig :: Prop obj name -> sig


objProp name obj = appByName (Proxy::Proxy "self") obj $ getProp name obj

