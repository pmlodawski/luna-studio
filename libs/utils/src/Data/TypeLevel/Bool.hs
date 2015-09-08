---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE PolyKinds          #-}

module Data.TypeLevel.Bool where

import Data.Typeable
import Prelude


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------



type family And a b where
  And True True = True
  And a    b    = False

type family If (cond :: Bool) (a :: k) (b :: k) :: k where
  If True  a b = a
  If False a b = b

type family (:==) (a :: k) (b :: l) where
    a :== a = True
    a :== b = False