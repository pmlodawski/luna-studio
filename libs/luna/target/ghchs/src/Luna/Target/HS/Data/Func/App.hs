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

--{-# LANGUAGE DysfunctionalDependencies #-}



module Luna.Target.HS.Data.Func.App where

import           Data.Typeable (Proxy, Typeable)
import           GHC.TypeLits


import           Flowbox.Utils


----------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------

--class AppNext val a b | val a -> b where
--    appNext :: val -> a -> b

----------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------


newtype AppH fptr args = AppH (fptr, args) deriving (Show, Eq, Typeable, Functor)
appH fptr args = AppH (fptr, args)

fromAppH (AppH a) = a

--newtype LamH lam args = LamH (lam, args) deriving (Show, Eq, Typeable)
--lamH lam args = LamH (lam, args)

--fromLamH (LamH a) = a

----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------

--appByName' name val (AppH (fptr, args)) = AppH (fptr, appArgByName name val args)
--appNext' val (AppH (fptr, args)) = AppH (fptr, appNextArg val args)

----appByName name val = (fmap.fmap) $ appByName' name val
----appNext val = (fmap.fmap) $ appNext' val

--appByName name val = fmap $ appByName' name val
--appNext val = fmap $ appNext' val

--appByName' name val fptr = fmap (appArgByName name val) fptr
--appNext' val fptr = fmap (appNextArg val) fptr


--instance  (AppNextArg val args argsout) =>AppNext val (AppH fptr args) (AppH fptr argsout)  where
--    appNext val (AppH (fptr, args)) = AppH (fptr, appNextArg val args)

--instance  (AppNextArg val args argsout) =>AppNext val (LamH lam args) (LamH lam argsout)  where
--    appNext val (LamH (lam, args)) = LamH (lam, appNextArg val args)
