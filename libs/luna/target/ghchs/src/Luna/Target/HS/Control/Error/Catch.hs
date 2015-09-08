---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}


{-# LANGUAGE DysfunctionalDependencies #-}




module Luna.Target.HS.Control.Error.Catch where

import Data.Typeable
import Luna.Target.HS.Control.Error.Data
import Luna.Target.HS.Control.Error.Raise

import Control.PolyApplicative

------------------------------------------------------------------------
-- Data types
------------------------------------------------------------------------

data ReRaise a = ReRaise deriving Show


------------------------------------------------------------------------
-- Type classes
------------------------------------------------------------------------

class Catch e fix val out | e fix val -> out where
    catch :: (e -> fix) -> val -> out


class Catch2 e fix val out | e fix val -> out where
    catch2 :: (e -> fix) -> val -> out


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

-- === basic catching === --

instance  (ea~a) =>Catch e (m ea) (Safe a) (Safe a)  where catch _ = id

instance  (a~ea, Catch e (Safe a) (base a) (mout a), Monad mout, out~mout a) =>Catch e (Safe ea) (UnsafeBase base e a) out  where
    catch f a = case a of
        UnsafeValue a -> return a
        Error       e -> return . fromSafe $ f e
        UnsafeOther o -> catch f o

instance  (a~ea, Catch e1 (Safe a) (base a) (dstBase a), out~UnsafeBase dstBase e2 a) =>Catch e1 (Safe ea) (UnsafeBase base e2 a) out  where
    catch f sa = case sa of
        UnsafeValue a -> UnsafeValue a
        Error       e -> Error e
        UnsafeOther o -> UnsafeOther $ catch f o

-- === re-raising === --

instance  (a~ea) =>Catch e1 (ReRaise ea) (UnsafeBase base e2 a) (UnsafeBase base e2 a)  where
    catch _ = id


---- === nested raising ===

instance  (ea~a, out~UnsafeBase base e2 a) =>Catch e1 (UnsafeBase base e2 ea) (UnsafeBase base e1 a) out  where
    catch f sa = case sa of
        UnsafeValue a -> UnsafeValue a
        Error       e -> f e
        UnsafeOther o -> UnsafeOther o


instance  (ea~a, Catch e1 (UnsafeBase base e2 a) (base a) (dstBase a), out~UnsafeBase dstBase e3 a) =>Catch e1 (UnsafeBase base e2 ea) (UnsafeBase base e3 a) out  where
    catch f sa = case sa of
        UnsafeValue a -> UnsafeValue a
        Error       e -> Error e
        UnsafeOther o -> UnsafeOther $ catch f o


instance  (ea~a, out~UnsafeBase dstBase e2 a, PolyApplicative (UnsafeBase base3 e2) (UnsafeBase base2 e2) (UnsafeBase dstBase e2)) =>Catch e1 (UnsafeBase base2 e2 ea) (UnsafeBase base3 e1 a) out  where
    catch f sa = case sa of
        UnsafeValue a -> UnsafeValue a
        Error       e -> (UnsafeValue id :: UnsafeBase base3 e2 (a->a)) <<*>> (f e :: (UnsafeBase base2 e2 a))
        --UnsafeOther o -> UnsafeOther $ catch f o

class PolyApplicative' m1 m2 m3 | m1 m2 -> m3 where
    (<<*>>~) :: m1 (a -> b) -> m2 a -> m3 b

--instance  (ea~a, Catch e1 (UnsafeBase base e2 a) (base a) (dstBase a), out~UnsafeBase dstBase e3 a) =>Catch e1 (UnsafeBase base2 e2 ea) (UnsafeBase base3 e3 a) out  where
--    catch f sa = case sa of
--        UnsafeValue a -> UnsafeValue a
--        Error       e -> Error e
--        UnsafeOther o -> UnsafeOther $ catch f o


class Unify m1 m2 where
    unify :: m1 a -> m2 a



    --instance Unify Safe (UnsafeBase base e) where
    --    unify = UnsafeValue . fromSafe

    ----instance  (a1~a2) =>Unify (UnsafeBase Safe NoError a1) (UnsafeBase base e a2)  where
    ----    unify (UnsafeValue a) = UnsafeValue a

    ----instance  (a1~a2, Unify (UnsafeBase s1 e1 a2) (UnsafeBase base e a2)) =>Unify (UnsafeBase (UnsafeBase s1 e1) NoError a1) (UnsafeBase base e a2)  where
    ----    unify sa = case sa of
    ----        UnsafeValue a -> UnsafeValue a
    ----        UnsafeOther o -> unify o

    ----instance  (a1~a2) =>Unify (UnsafeBase base e a1) (Safe a2)  where
    ----    unify = 

    --data NoError = NoError deriving Show

    --instance Catch2 e1 x (Safe a) (UnsafeBase Safe NoError a) where
    --    catch2 _ = UnsafeValue . fromSafe

    --instance  (a1~a2, out~mout a1, Monad mout, Unify m1 mout, Catch2 e1 (m1 a1) (base a2) (mout a2)) =>Catch2 e1 (m1 a1) (UnsafeBase base e1 a2) out  where
    --    catch2 f sa = case sa of
    --        UnsafeValue a -> return a
    --        Error       e -> unify $ f e
    --        UnsafeOther o -> catch2 f o


    --instance  (Catch2 e1 x (base a) (base0 a), out~UnsafeBase base0 e2 a) =>Catch2 e1 x (UnsafeBase base e2 a) out  where
    --    catch2 f sa = case sa of
    --        UnsafeValue a -> UnsafeValue a
    --        Error       e -> Error e
    --        UnsafeOther o -> UnsafeOther $ catch2 f o


    ----instance Catch2 e1 x (UnsafeBase base e3 a) out where
    ----    catch2 f sa = case sa of
    ----        UnsafeValue a -> UnsafeValue a
    ----        Error       e -> Error e
    ----        UnsafeOther o -> UnsafeOther $ catch2 f o

    --v = Safe 0
    --data E1 = E1 deriving (Show, Typeable, Eq)
    --data E2 = E2 deriving (Show, Typeable, Eq)
    --data E3 = E3 deriving (Show, Typeable, Eq)
    --data E4 = E4 deriving (Show, Typeable, Eq)
    --data E5 = E5 deriving (Show, Typeable, Eq)