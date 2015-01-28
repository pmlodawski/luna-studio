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

import qualified Luna.Target.HS.Data.Func.Args7 as Args7
import qualified Luna.Target.HS.Data.Func.Args9 as Args9
import Control.Category.Dot


import Data.Typeable
import Type.BaseType

----------------------------------------------------------------------------------
-- Mem (proxy datatype)
----------------------------------------------------------------------------------

data Mem obj (name :: Symbol) = Mem (Proxy obj) (Proxy name) deriving (Typeable)

instance Show (Mem obj name) <= (Typeable obj, KnownSymbol name) where
    show (Mem obj name) = "Mem " ++ show (typeOf obj) ++ " " ++ show (symbolVal name)

----------------------------------------------------------------------------------
-- HasMem
----------------------------------------------------------------------------------




class MemberProvider obj name argRep f | obj name argRep -> f where
    getMember :: Mem obj name -> argRep -> f



objPtr :: m base s a -> out <= (Env base, Safety s, BaseType (Proxy a) out, out~Proxy b)
objPtr el = Proxy

memPtr :: Proxy name -> m base s a -> Mem obj name <= (Env base, Safety s, BaseType (Proxy a) (Proxy obj))
memPtr name obj = Mem (objPtr obj) name

getMem name obj = val . appH ptr $ Args9.empty where
    ptr = memPtr name obj


addArg' = (fmap.fmap) . Args9.addArg
appNext = addArg' . Args9.upArg
appByName = addArg' `dot2` Args9.npArg



--member name obj = appByName obj (Proxy::Proxy "self") $ getMem name obj

member name obj = appByName obj (Proxy::Proxy "self") $ getMem name obj


--mkFunc :: Mem cls name -> f -> Args7.Func (Args7.SigOf cls name) f
--mkFunc _ = Args7.Func



