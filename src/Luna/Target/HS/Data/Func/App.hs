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

module Luna.Target.HS.Data.Func.App where

import GHC.TypeLits
import Data.Typeable (Typeable, Proxy)


import Flowbox.Utils

import Luna.Target.HS.Data.Func.Args

----------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------

--class AppNext val a b | val a -> b where
--    appNext :: val -> a -> b

----------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------


newtype AppH fptr args = AppH (fptr, args) deriving (Show, Eq, Typeable)
appH fptr args = AppH (fptr, args)

fromAppH (AppH a) = a

--newtype LamH lam args = LamH (lam, args) deriving (Show, Eq, Typeable)
--lamH lam args = LamH (lam, args)

--fromLamH (LamH a) = a

----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------

appByName' name val (AppH (fptr, args)) = AppH (fptr, appArgByName name val args)
appNext' val (AppH (fptr, args)) = AppH (fptr, appNextArg val args)

appByName name val = (fmap.fmap) $ appByName' name val
appNext val = (fmap.fmap) $ appNext' val

--appByName' name val fptr = fmap (appArgByName name val) fptr
--appNext' val fptr = fmap (appNextArg val) fptr


--instance AppNext val (AppH fptr args) (AppH fptr argsout) <= (AppNextArg val args argsout) where
--    appNext val (AppH (fptr, args)) = AppH (fptr, appNextArg val args)

--instance AppNext val (LamH lam args) (LamH lam argsout) <= (AppNextArg val args argsout) where
--    appNext val (LamH (lam, args)) = LamH (lam, appNextArg val args)