---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA as C

import Flowbox.Prelude

main :: IO ()
main = do
    putStrLn "- - - = = =   Bezier Test   = = = - - -"

    let a = A.use $ fromList (Z :. 3) [1,2,3] :: Acc (Vector Int)

    print $ run a

    return ()
