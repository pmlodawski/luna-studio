---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Send functions from this module as pull request to Accelerate?
module Flowbox.Graphics.Utils.Accelerate where

import qualified Data.Array.Accelerate as A

import Flowbox.Prelude hiding (head, last)




index3 :: A.Exp Int -> A.Exp Int -> A.Exp Int -> A.Exp A.DIM3
index3 x y z = A.lift $ A.Z A.:. x A.:. y A.:. z

-- | Sequential map function
smap :: forall e sh. (A.Elt e, A.Shape sh) => (A.Exp e -> A.Exp e) -> A.Acc (A.Array sh e) -> A.Acc (A.Array sh e)
smap f arr = A.reshape inputSize $ A.asnd $ A.awhile condition step initialState
  where inputSize    = A.shape arr
        condition v  = A.unit $ A.the (A.afst v) A.<* n
        initialState = A.lift (A.unit $ A.constant 0, emptyVector)

        n = A.size arr

        step :: A.Acc (A.Scalar Int, A.Vector e) -> A.Acc (A.Scalar Int, A.Vector e)
        step (A.unlift -> (it, acc) :: (A.Acc (A.Scalar Int), A.Acc (A.Vector e))) =
            A.lift (A.unit (A.the it + 1),
                    acc A.++ (A.reshape (A.index1 1) $ A.unit (f $ arr A.!! (A.the it))))


emptyVector :: (A.Elt e) => A.Acc (A.Vector e)
emptyVector = A.use $ A.fromList (A.Z A.:. (0 :: Int)) []

head :: (A.Elt e, A.Shape sh) => A.Acc (A.Array sh e) -> A.Exp e
head vec = vec A.!! 0

last :: (A.Elt e, A.Shape sh) => A.Acc (A.Array sh e) -> A.Exp e
last vec = vec A.!! (A.size vec - 1)
