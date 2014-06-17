---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Utils where

import           Flowbox.Prelude                      as P
import           Flowbox.Math.Matrix                  as M

import           Data.Array.Accelerate                as A (truncate)
import           Data.Array.Accelerate.IO
import           Data.Array.Accelerate.CUDA

testSaveRGBA :: FilePath -> Matrix2 Double -> Matrix2 Double -> Matrix2 Double -> Matrix2 Double -> IO ()
testSaveRGBA filename r g b a = writeImageToBMP filename $ compute' run $ M.map packRGBA32 $ zip4 rChan gChan bChan aChan
    where rChan = M.map (A.truncate . (* 255.0)) r
          gChan = M.map (A.truncate . (* 255.0)) g
          bChan = M.map (A.truncate . (* 255.0)) b
          aChan = M.map (A.truncate . (* 255.0)) a

testSaveChan :: FilePath -> Matrix2 Double -> IO ()
testSaveChan filename a = testSaveRGBA filename a a a a
