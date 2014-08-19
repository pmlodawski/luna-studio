---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}


!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Control.Error.Data where

import Control.Applicative  
import Data.Typeable

import Control.PolyApplicative
import Control.PolyMonad
import Control.Monad.Shuffle
import Flowbox.Utils


------------------------------------------------------------------------
-- Data Types
------------------------------------------------------------------------

newtype Safe a = Safe a deriving (Show, Typeable, Eq)

data UnsafeBase base err val = UnsafeValue val
                             | Error       err
                             | UnsafeOther (base val)
                             deriving (Show, Eq
#if __GLASGOW_HASKELL__ >= 708
                                      , Typeable
#endif
                                      )

type Unsafe = UnsafeBase Safe


------------------------------------------------------------------------
-- Data operators
------------------------------------------------------------------------

fromSafe :: Safe a -> a
fromSafe (Safe a) = a


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

-- == Functor == --

instance Functor Safe where
    fmap f (Safe a) = Safe (f a)


instance Functor (UnsafeBase base err) <= Functor base where
  fmap f a = case a of
      UnsafeValue a -> UnsafeValue $ f a
      Error       e -> Error e
      UnsafeOther b -> UnsafeOther $ fmap f b


-- == Monad == --

instance Monad Safe where
    return = Safe
    (Safe a) >>= f = f a

instance Monad (UnsafeBase base err) <= (PolyMonad base (UnsafeBase base err) (UnsafeBase base err)) where
    return = UnsafeValue
    v >>= f = v >>=~ f

-- == Applicative == --   

instance Applicative Safe where
    pure = Safe
    (Safe f) <*> Safe a = Safe $ f a

instance Applicative (UnsafeBase base err) <= (Functor base, (PolyApplicative (UnsafeBase base err) (UnsafeBase base err) (UnsafeBase base err))) where
    pure = UnsafeValue
    a <*> b = a <<*>> b


-- == PolyMonad == --

instance PolyMonad Safe Safe Safe where
    (Safe a) >>=~ f = f a

instance PolyMonad Safe (UnsafeBase base err) (UnsafeBase base err) where
    (Safe a) >>=~ f = f a

instance PolyMonad (UnsafeBase base err) Safe (UnsafeBase base err) <= Functor base where
    a >>=~ f = fmap (fromSafe . f) a

-- FIXME!!! PolyMonad should be defined for distinct base nd err types!
instance PolyMonad (UnsafeBase base1 err1) (UnsafeBase base2 err2) (UnsafeBase base2 err2) where
    a >>=~ f = undefined


--instance PolyMonad (UnsafeBase base err) (UnsafeBase base err) (UnsafeBase base err) <= (PolyMonad base (UnsafeBase base err) (UnsafeBase base err)) where
--    a >>=~ f = case a of
--        UnsafeValue v -> f v
--        Error       e -> Error e
--        UnsafeOther o -> o >>=~ f


-- == PolyApplicative == --

instance PolyApplicative Safe Safe Safe where
    Safe f <<*>> Safe a = Safe (f a)

instance PolyApplicative Safe (UnsafeBase base e) (UnsafeBase base e) <= (PolyApplicative Safe base base) where
    Safe f <<*>> sa = case sa of
        UnsafeValue a -> UnsafeValue $ f a
        Error       e -> Error e
        UnsafeOther o -> UnsafeOther $ Safe f <<*>> o

instance PolyApplicative (UnsafeBase base e) Safe (UnsafeBase base e) <= (PolyApplicative base Safe base) where
    sf <<*>> Safe b = case sf of
        UnsafeValue f -> UnsafeValue $ f b
        Error       e -> Error e
        UnsafeOther o -> UnsafeOther $ o <<*>> Safe b

--------------------------
--instance PolyApplicative (UnsafeBase base e1) (UnsafeBase Safe e2) (UnsafeBase dstBase e1) <= (PolyApplicative base (UnsafeBase Safe e2) dstBase, Monad base) where
--    (<<*>>) (sf :: UnsafeBase base e1 (a->b)) sa = case sf of
--        UnsafeValue f -> case sa of
--            UnsafeValue a -> UnsafeValue $ f a
--            Error e -> UnsafeOther $ (<<*>>) (return f :: base (a->b)) sa
--            UnsafeOther (Safe a) -> UnsafeValue $ f a
--        Error e -> Error e
--        UnsafeOther o -> UnsafeOther $ (<<*>>) o sa

-- vvv potrzebne?
instance PolyApplicative (UnsafeBase base e) (UnsafeBase base e) (UnsafeBase base e) <= (PolyApplicative Safe base base, PolyApplicative base Safe base, PolyApplicative base base base) where
    sf <<*>> sa = case sf of
        UnsafeValue f -> case sa of
            UnsafeValue a -> UnsafeValue $ f a
            Error       e -> Error e
            UnsafeOther o -> UnsafeOther $ Safe f <<*>> o
        Error       e -> Error e
        UnsafeOther o -> case sa of
            UnsafeValue a  -> UnsafeOther $ o <<*>> Safe a
            Error       e  -> Error e
            UnsafeOther o' -> UnsafeOther $ o <<*>> o'

instance PolyApplicative (UnsafeBase base1 e) (UnsafeBase base2 e) (UnsafeBase dstBase e) <= (Monad base1, Monad base2, PolyApplicative base1 base2 dstBase) where
    (sf :: UnsafeBase base1 e (a->b)) <<*>> (sa :: UnsafeBase base2 e a) = case sf of
        UnsafeValue f -> case sa of
            UnsafeValue a -> UnsafeValue $ f a
            Error       e -> Error e
            UnsafeOther o -> UnsafeOther $ (return f :: base1 (a->b)) <<*>> o
        Error       e -> Error e
        UnsafeOther o -> case sa of
            UnsafeValue a  -> UnsafeOther $ o <<*>> (return a :: base2 a)
            Error       e  -> Error e
            UnsafeOther o' -> UnsafeOther $ o <<*>> o'

instance PolyApplicative (UnsafeBase base1 e1) (UnsafeBase base2 e2) out  <= (PolyApplicative base1 (UnsafeBase Safe e2) dstBase, PolyApplicative (UnsafeBase dstBase e1) base2 out, Functor dstBase, PolyApplicative dstBase Safe dstBase, Functor base1, Monad base1, Monad base2) where
    (<<*>>) (sf :: UnsafeBase base1 e1 (a->b)) (sa :: UnsafeBase base2 e2 a) = case sa of
        UnsafeValue a -> (fmap const $ liftTrans sf (UnsafeValue a :: UnsafeBase Safe e2 a))    <<*>> (return undefined :: base2 a)
        Error       e -> (fmap const $ liftTrans sf (Error e :: UnsafeBase Safe e2 a))          <<*>> (return undefined :: base2 a)
        UnsafeOther o -> (liftTrans (fmap const sf) (return undefined :: UnsafeBase Safe e2 a)) <<*>> (o :: base2 a)


liftTrans :: (PolyApplicative base (UnsafeBase Safe t) dstBase, Monad base) => UnsafeBase base e1 (a -> b) -> UnsafeBase Safe t a -> UnsafeBase dstBase e1 b
liftTrans (sf :: UnsafeBase base e1 (a->b)) sa = case sf of
    UnsafeValue f -> case sa of
        UnsafeValue a -> UnsafeValue $ f a
        Error       e -> UnsafeOther $ (return f :: base (a->b)) <<*>> sa
        UnsafeOther (Safe a) -> UnsafeValue $ f a
    Error       e -> Error e
    UnsafeOther o -> UnsafeOther $ o <<*>> sa

    
-- == Shuffle == --

instance Shuffle Safe a <= Functor a where
    shuffle = fmap Safe . fromSafe

instance Shuffle (UnsafeBase base err) a <= (Functor a, Monad a, Shuffle base a) where
    shuffle = \case
        UnsafeValue val  -> fmap UnsafeValue val
        Error e          -> return $ Error e
        UnsafeOther base -> fmap UnsafeOther $ shuffle base