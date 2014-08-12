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

module Luna.Target.HS.Data.Func.Func where

import GHC.TypeLits
import Data.Typeable (Typeable, Proxy)


import Flowbox.Utils

import Luna.Target.HS.Data.Func.Args
--import qualified Luna.Target.Func.HS.Args as Args

----------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------

newtype AppH fptr args = AppH (fptr, args) deriving (Show, Eq, Typeable)
appH fptr args = AppH (fptr, args)

fromAppH (AppH a) = a
----------------------------------------------------------------------------------
-- Func
----------------------------------------------------------------------------------

--class Func fptr args out | fptr args -> out where
--    getFunc :: fptr -> args -> out

class Func fptr args out | fptr args -> out where
    getFunc :: fptr -> args -> (args -> out)

--class FuncD fptr func | fptr -> func where
--    getFuncD :: fptr -> func

--class FuncD2 fptr args func | fptr args -> func where
--    getFuncD2 :: fptr -> args -> func

--call (AppH(fptr, args)) = getFunc fptr args' where
--    args' = readArgs args

call (AppH(fptr, args)) = (getFunc fptr args') args' where
    args' = readArgs args

----------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------

--instance AppNextArg val (AppH fptr args) (AppH fptr out) <= (AppNextArg val args out) where
--    appNextArg val (AppH (fptr, args)) = AppH (fptr, appNextArg val args)

--instance AppArgByName name val (AppH fptr args) (AppH fptr out) <= (AppArgByName name val args out) where
--    appArgByName name val (AppH (fptr, args)) = AppH (fptr, appArgByName name val args)


--appByName = appArgByName
--appNext   = appNextArg

appByName name val (AppH (fptr, args)) = AppH (fptr, appArgByName name val args)
appNext val (AppH (fptr, args)) = AppH (fptr, appNextArg val args)

appByName' name val fptr = fmap (appArgByName name val) fptr
appNext' val fptr = fmap (appNextArg val) fptr

curryByName = matchCall `dot3` appByName
curryNext   = matchCall `dot2` appNext


----------------------------------------------------------------------------------
-- MatchCall
----------------------------------------------------------------------------------


class MatchCallProto (allArgs :: Bool) obj out | allArgs obj -> out where
    matchCallProto :: Proxy allArgs -> obj -> out

instance MatchCallProto False a a where
    matchCallProto _ = id

instance MatchCallProto True (AppH fptr args) out <= (ReadArgs args margs, Func fptr margs out) where
    matchCallProto _ = call


class MatchCall obj out | obj -> out where
    matchCall :: obj -> out

instance MatchCall (AppH fptr args) out <= (MatchCallProto flag (AppH fptr args) out, AllArgs args flag) where
    matchCall = matchCallProto (undefined :: Proxy flag)
