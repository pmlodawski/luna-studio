{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

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

class Condition cond a where
    if' :: cond -> a -> a -> a

instance Condition Bool a where
    if' True  a _ = a
    if' False _ a = a

instance Elt a => Condition (Exp Bool) (Exp a) where
    if' = A.cond

type family Boolean a

class (Condition (Boolean a) a, Eq (Boolean a) a) => Ord ord a where
    compare :: a -> a -> ord

    infix 4 <
    (<) :: a -> a -> Boolean ord

    infix 4 >=
    (>=) :: a -> a -> Boolean ord 

    infix 4 >
    (>) :: a -> a -> Boolean ord

    infix 4 <=
    (<=) :: a -> a -> Boolean ord

    max :: a -> a -> a

    min :: a -> a -> a

class (Boolean a ~ bool) => Eq bool a where
    infix 4 ==
    (==) :: a -> a -> bool

    infix 4 /=
    (/=) :: a -> a -> bool

class Logical bool where
    infixr 3 &&
    (&&) :: bool -> bool -> bool

    infixr 2 ||
    (||) :: bool -> bool -> bool

    not :: bool -> bool
