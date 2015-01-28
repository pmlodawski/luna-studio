---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
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

--{-# LANGUAGE DysfunctionalDependencies #-}

!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Data.Func.Call where

import GHC.TypeLits
import Data.Typeable (Typeable, Proxy)


import Flowbox.Utils

import Control.PolyMonad
import Luna.Target.HS.Data.Func.App
import Luna.Target.HS.Data.Struct
import Control.Monad.Shuffle
import Luna.Target.HS.Data.Func.Func
import Luna.Target.HS.Data.Func.Lam

import qualified Luna.Target.HS.Data.Func.Args2 as Args2
import qualified Luna.Target.HS.Data.Func.Args7 as Args7
import qualified Luna.Target.HS.Data.Func.Args9 as Args9

import Luna.Target.HS.Utils.MonoType (monoType, TVar, Analyze)

----------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------




----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------


callH (AppH(fptr, args)) = Args9.appDefaults . Args9.appArgs args $ Args9.func sig f where
    (sig,f) = getMember fptr (monoType args)
call = polyJoin . fmap callH

