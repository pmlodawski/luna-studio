---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Math.Matrix where

import Data.Array.Accelerate

import Flowbox.Prelude hiding (use)



data Matrix ix a = Raw (Array ix a)
                 | Delayed (Acc (Array ix a))
                 deriving (Show)

type Backend ix a = Acc (Array ix a) -> Array ix a

-- ==== HELPERS

accMatrix :: (Elt a, Shape ix) => Matrix ix a -> Acc (Array ix a)
accMatrix mat = case mat of
    Raw     m -> use m
    Delayed m -> m

compute :: Backend ix a -> Matrix ix a -> Matrix ix a
compute backend mat = Raw $ case mat of
    Raw     m -> m
    Delayed m -> backend m

