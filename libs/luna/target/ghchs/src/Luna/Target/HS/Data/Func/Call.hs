---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# LANGUAGE DysfunctionalDependencies #-}



module Luna.Target.HS.Data.Func.Call where

import           Data.Typeable                  (Proxy, Typeable)
import           GHC.TypeLits


import           Flowbox.Utils

import           Control.Monad.Shuffle
import           Control.PolyMonad
import           Luna.Target.HS.Data.Func.App
import           Luna.Target.HS.Data.Func.Func
import           Luna.Target.HS.Data.Func.Lam
import           Luna.Target.HS.Data.Struct

import           Luna.Target.HS.Data.Func.Args9
import qualified Luna.Target.HS.Data.Func.Args9 as Args

import           Luna.Target.HS.Utils.MonoType  (Analyze, TVar, monoType)

----------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------




----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------

--callH :: ( MemberProvider obj name argRep (sig1, f1), AppArgs b (ArgsKind sig1) sig1 f1 k1 sig f, AppDefaults k1 f sig c
--         , Reverse a b, Analyze a argRep)
--      => AppH (Mem obj name) a -> c
--callH (AppH(fptr, args)) = appDefaults . appArgs args $ func sig f where
--    (sig,f) = getMember fptr (monoType args)

--callL :: (Reverse a b, AppArgs b k1 sig1 f1 k sig f, AppDefaults k f sig c)
--      => AppH (Lam (Func k1 sig1 f1)) a -> c
--callL (AppH (Lam f, args)) = appDefaults . appArgs args $ f


--class Call' h args r | h args -> r where
--    call' :: AppH h args -> r

--instance (Reverse a b, AppArgs b k1 sig1 f1 k sig f, AppDefaults k f sig c, fc~Func k1 sig1 f1)
--      => Call' (Lam fc) a c where
--    call' (AppH (Lam f, args)) = appDefaults . appArgs args $ f

--instance (Call' (Lam (Func (ArgsKind sig) sig f)) args r, MemberProvider obj name argRep (sig, f), Analyze args argRep)
--      => Call' (Mem obj name) args r where
--    call' (AppH (fptr, args)) = call' $ AppH (Lam (func sig f), args) where
--        (sig,f) = getMember fptr (monoType args)

call = polyJoin . fmap call'


--call2 :: (MemberProvider obj name argRep (sig1, f1), PolyMonad m1 m2 m3, AppArgs b (ArgsKind sig1) sig1 f1 k1 sig f, AppDefaults k1 f sig (m2 a), Reverse' a1 () b, Functor m1)
--      => (argRep :: *) -> m1 (AppH (Mem (obj :: k) (name::Symbol)) a1) -> m3 (a :: *)
call2 x = polyJoin . fmap (call2' x)


call' (AppH (fptr, args)) = appDefaults . appArgs args $ func sig f where
        (sig,f) = getMember fptr (monoType args)

--call2' :: (MemberProvider obj name argRep (sig1, f1), AppArgs b (ArgsKind sig1) sig1 f1 k1 sig f, AppDefaults k1 f sig c, Reverse' a () b)
--       => (argRep :: *) -> AppH (Mem (obj :: k) (name::Symbol)) a -> (c :: *)
call2' x (AppH (fptr, args)) = appDefaults . appArgs args $ func sig f where
        (sig,f) = getMember fptr x


call3' x (AppH (fptr, args)) = func sig f where
        (sig,f) = getMember fptr x



