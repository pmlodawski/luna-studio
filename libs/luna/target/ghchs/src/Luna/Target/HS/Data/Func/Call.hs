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
import Luna.Target.HS.Data.Func.Args
import Luna.Target.HS.Data.Func.App
import Luna.Target.HS.Data.Struct
import Control.Monad.Shuffle
import Luna.Target.HS.Data.Func.Func
import Luna.Target.HS.Data.Func.Lam

import qualified Luna.Target.HS.Data.Func.Args2 as Args2

----------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------

class MatchCallProto (allArgs :: Bool) obj out | allArgs obj -> out where
    matchCallProto :: Proxy allArgs -> obj -> out

class MatchCall obj out | obj -> out where
    matchCall :: obj -> out

class Call a b | a -> b where
    call' :: a -> b

class Call2 a b | a -> b where
    call2' :: a -> b


class Call3 ptr f where
    call3' :: AppH ptr (f -> out) -> out


----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------

instance Call (AppH (Mem (base :: k) name) args) out <= (Func base name argsout out, ReadArgs args argsout) where
    call' (AppH(fptr, args)) = (getFunc fptr args') args' where
        args' = readArgs args

instance FuncProvider base name f => Call2 (AppH (Mem (base :: k) name) (f -> out)) out where
    call2' (AppH(fptr, f)) = f (getFunc2 fptr f) where
        --args' = readArgs args

instance FuncProvider base name f => Call3 (Mem (base :: k) name) f where
    call3' (AppH(fptr, f)) = f (getFunc2 fptr f)

call3H' (AppH(fptr, f)) = f (getFunc2 fptr f)

instance Call (AppH (Lam lam) args) out <= (lam~(argsout -> out), ReadArgs args argsout) where
    call' (AppH(Lam lam, args)) = lam (readArgs args)

curryByName = matchCall `dot3` appByName
curryNext   = matchCall `dot2` appNext

--call = shuffleJoin . (fmap.fmap) call'

call = polyJoin . fmap call'

call2 :: (Call2 a1 (m2 a), PolyMonad m1 m2 m3, Functor m1) => m1 a1 -> m3 a
call2 = polyJoin . fmap (call2')
call3 = polyJoin . fmap (call3' . fmap Args2.runFuncTrans)
call4 = polyJoin . fmap (call3H' . fmap Args2.runFuncTrans)

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
