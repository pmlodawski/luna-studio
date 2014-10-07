---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Graphics.Utils.Linear where

import qualified Data.Array.Accelerate as A
import           Foreign.C.Types       (CFloat, CDouble)
import           Linear                hiding (Epsilon, nearZero, inv33)
import           Linear.Accelerate     ()

import Flowbox.Prelude



class (A.IsNum a) => AccEpsilon a where
    nearZero :: A.Exp a -> A.Exp Bool

instance AccEpsilon Float where
    nearZero a = abs a A.<=* 1e-6

instance AccEpsilon Double where
    nearZero a = abs a A.<=* 1e-12

instance AccEpsilon CFloat where
    nearZero a = abs a A.<=* 1e-6

instance AccEpsilon CDouble where
    nearZero a = abs a A.<=* 1e-12

-- | Returns True if inversion has succeeded
inv33 :: forall a. (AccEpsilon a, A.Elt a, A.IsFloating a)
      => A.Exp (M33 a) -> A.Exp (Bool, M33 a)
inv33 t = A.cond (nearZero det)
          (A.lift (False, m))
          (A.lift (True, (1 / det) *!! V3 (V3 a' b' c')
                                          (V3 d' e' f')
                                          (V3 g' h' i')))
    where (V3 x y z) = A.unlift t :: V3 (A.Exp (V3 a))
          x'@(V3 a b c) = A.unlift x :: V3 (A.Exp a)
          y'@(V3 d e f) = A.unlift y :: V3 (A.Exp a)
          z'@(V3 g h i) = A.unlift z :: V3 (A.Exp a)
          m = V3 x' y' z'
          -- ^^^ Accelerate y u do dis


          a' = cofactor (e,f,h,i)
          b' = cofactor (c,b,i,h)
          c' = cofactor (b,c,e,f)
          d' = cofactor (f,d,i,g)
          e' = cofactor (a,c,g,i)
          f' = cofactor (c,a,f,d)
          g' = cofactor (d,e,g,h)
          h' = cofactor (b,a,h,g)
          i' = cofactor (a,b,d,e)
          cofactor (p,q,r,s) = det22 (V2 (V2 p q) (V2 r s))
          det = det33 m
