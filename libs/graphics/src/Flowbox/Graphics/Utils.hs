---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Utils where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import           Flowbox.Prelude       as P



nonIntRem :: (A.Elt e, A.IsFloating e) => Exp e -> Exp e -> Exp e
nonIntRem x y = x - (y * (A.fromIntegral (A.truncate (x / y) :: Exp Int)))

nonIntDiv :: (A.Elt e, A.IsFloating e) => Exp e -> Exp e -> Exp e
nonIntDiv x y = A.fromIntegral (A.truncate (x / y) :: Exp Int)

data Size a = Size {w :: a, h :: a}
