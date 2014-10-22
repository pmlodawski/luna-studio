---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}

module Flowbox.Graphics.Prelude (
    module Flowbox.Graphics.Prelude,
    module PE
) where

import Flowbox.Prelude as PE hiding (
    lift, unlift, 
    Ord(..), Eq(..),
    (&&), (||), not
    )

import qualified Flowbox.Prelude       as P
import           Data.Array.Accelerate as A



data HPlain
data HAccelerate

type family BooleanT t a
type instance BooleanT HAccelerate (Exp a) = Exp Bool
type instance BooleanT HPlain a            = Bool

type family Boolean a where
    Boolean a = BooleanT (InferH a) a

type family InferH a where
    InferH (Exp a) = HAccelerate
    InferH a       = HPlain

-- == Condition class ==
type family Condition a where
    Condition a = ConditionC (InferH a) a

class (t ~ InferH a) => ConditionC t a where
    if' :: BooleanT t a -> a -> a -> a

instance (InferH a ~ HPlain) => ConditionC HPlain a where
    if' True  a _ = a
    if' False _ a = a

instance Elt a => ConditionC HAccelerate (Exp a) where
    if' = A.cond

-- == Eq class ==
type family Eq a where
    Eq a = EqC (InferH a) a

class t ~ InferH a => EqC t a where
    infix 4 ==
    (==) :: a -> a -> BooleanT t a

    infix 4 /=
    (/=) :: a -> a -> BooleanT t a

instance (P.Eq a, InferH a ~ HPlain) => EqC HPlain a where
    (==) = (P.==)
    (/=) = (P./=)

instance (Elt a, IsScalar a) => EqC HAccelerate (Exp a) where
    (==) = (==*)
    (/=) = (/=*)

-- == Ord class ==
type family Ord a where
    Ord a = OrdC (InferH a) a

class (t ~ InferH a, EqC t a) => OrdC t a where
    infix 4 <
    (<) :: a -> a -> BooleanT t a

    infix 4 >=
    (>=) :: a -> a -> BooleanT t a 

    infix 4 >
    (>) :: a -> a -> BooleanT t a

    infix 4 <=
    (<=) :: a -> a -> BooleanT t a

instance (P.Eq a, P.Ord a, InferH a ~ HPlain) => OrdC HPlain a where
    (<) = (P.<)
    (>) = (P.>)
    (>=) = (P.>=)
    (<=) = (P.<=)

instance (Elt a, IsScalar a) => OrdC HAccelerate (Exp a) where
    (<) = (A.<*)
    (>) = (A.>*)
    (>=) = (A.>=*)
    (<=) = (A.<=*)

min :: (Condition a, Ord a) => a -> a -> a
min a b = if' (a < b) a b

max :: (Condition a, Ord a) => a -> a -> a
max a b = if' (a > b) a b


class Logical bool where
    infixr 3 &&
    (&&) :: bool -> bool -> bool

    infixr 2 ||
    (||) :: bool -> bool -> bool

    not :: bool -> bool

instance Logical Bool where
    (&&) = (P.&&)
    (||) = (P.||)
    not = P.not

instance Logical (Exp Bool) where
    (&&) = (A.&&*)
    (||) = (A.||*)
    not = A.not
