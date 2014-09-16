---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Flowbox.Graphics.Utils.If where

import Flowbox.Prelude as P
import qualified Data.Array.Accelerate as A

import GHC.Exts

class Ifable cond a where
    type IfConstr cond a :: Constraint
    if' :: IfConstr cond a => cond -> a -> a -> a

--class IfableOrd ord a where
--    type OrdConstr a :: Constraint
--    type OrdBool ord :: *
--    compare :: OrdConstr a => a -> a -> ord

--    (<)  :: OrdConstr a => a -> a -> OrdBool ord infix 4
--    (>=) :: OrdConstr a => a -> a -> OrdBool ord infix 4
--    (>)  :: OrdConstr a => a -> a -> OrdBool ord infix 4
--    (<=) :: OrdConstr a =>  a -> a -> OrdBool ord infix 4

--    max :: a -> a -> a
--    min :: a -> a -> a

instance Ifable (A.Exp Bool) (A.Exp a) where
    type IfConstr (A.Exp Bool) (A.Exp a) = (A.Elt a)
    if' = A.cond

instance Ifable A.Bool a where
    type IfConstr Bool a = ()

    if' True a _ = a
    if' False _ b = b
