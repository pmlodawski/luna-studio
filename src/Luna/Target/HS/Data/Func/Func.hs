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
import Luna.Target.HS.Data.Func.App
import Luna.Target.HS.Data.Struct
import Control.Monad.Shuffle

----------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------

class Func base name args out | base name args -> out where
    getFunc :: Mem base name -> args -> (args -> out)

class MatchCallProto (allArgs :: Bool) obj out | allArgs obj -> out where
    matchCallProto :: Proxy allArgs -> obj -> out

class MatchCall obj out | obj -> out where
    matchCall :: obj -> out

class Call a b | a -> b where
    call' :: a -> b


----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------

--call (AppH(fptr, args)) = (getFunc fptr args') args' where
--    args' = readArgs args

instance Call (AppH (Mem base name) args) out <= (Func base name argsout out, ReadArgs args argsout) where
    call' (AppH(fptr, args)) = (getFunc fptr args') args' where
        args' = readArgs args

instance Call (AppH (Lam lam) args) out <= (lam~(argsout -> out), ReadArgs args argsout) where
    call' (AppH(Lam lam, args)) = lam (readArgs args)

curryByName = matchCall `dot3` appByName
curryNext   = matchCall `dot2` appNext

call = shuffleJoin . (fmap.fmap) call'

----------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------

instance MatchCallProto False a a where
    matchCallProto _ = id

instance MatchCallProto True (AppH (Mem base name) args) out <= (ReadArgs args margs, Func base name margs out) where
    matchCallProto _ = call'

---

instance MatchCall (AppH fptr args) out <= (MatchCallProto flag (AppH fptr args) out, AllArgs args flag) where
    matchCall = matchCallProto (undefined :: Proxy flag)
