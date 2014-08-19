---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Deprecated.Accelerate.Sequential where

import           Data.Array.Accelerate (Exp, Acc, Array, Vector)
import qualified Data.Array.Accelerate as A

import           Flowbox.Prelude                    as P

--emptyVector :: (A.Shape sh, A.Elt a) => sh -> Array sh a
--emptyArray sh = A.fromList sh []

emptyVector :: A.Elt a => Vector a
emptyVector = A.fromList (A.Z A.:. 1) []

invertVector :: A.Elt a => Acc (Vector a) -> Acc (Vector a)
invertVector vector = invertVector' (A.use emptyVector) vector
    where invertVector' acc vect = A.acond (A.null vect) acc
                                 $ invertVector' ((A.take 1 vect) A.++ acc) (A.tail vect)

map :: (A.Shape ix, A.Elt a, A.Elt b) => (Exp a -> Exp b) -> Acc (Array ix a) -> Acc (Array ix b)
map f arr = A.reshape sh flatArr'
    where sh = A.shape arr
          flatArr' = map' (A.use emptyVector) flatArr
          flatArr = A.flatten arr
          map' acc vect = A.acond (A.null vect) acc
                        $ map' ((A.flatten $ A.unit $ f (vect A.!! 0)) A.++ acc) (A.tail vect)
