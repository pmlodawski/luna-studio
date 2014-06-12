---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Utils where

import           Flowbox.Prelude                      as P
import           Data.Array.Accelerate                as A
import           Data.Array.Accelerate.IO
import           Data.Array.Accelerate.CUDA



testSaveAccRGB :: FilePath -> Acc (Array DIM2 Double) -> Acc (Array DIM2 Double) -> Acc (Array DIM2 Double) -> Acc (Array DIM2 Double) -> IO ()
testSaveAccRGB filename r g b a = writeImageToBMP filename $ run $ A.map packRGBA32 $ zip4 rChan gChan bChan aChan
    where
        rChan = A.map (A.truncate . (* 255.0)) r
        gChan = A.map (A.truncate . (* 255.0)) g
        bChan = A.map (A.truncate . (* 255.0)) b
        aChan = A.map (A.truncate . (* 255.0)) a

testSaveAccChan :: FilePath -> Acc (Array DIM2 Double) -> IO ()
testSaveAccChan filename a = testSaveAccRGB filename a a a a
