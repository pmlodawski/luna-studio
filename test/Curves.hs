---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Main where

import Data.Array.Accelerate    as A

#ifdef ACCELERATE_CUDA_BACKEND
import Data.Array.Accelerate.CUDA        (run)
#else
import Data.Array.Accelerate.Interpreter (run)
#endif

import Math.Coordinate.Cartesian (Point2(..))
import Flowbox.Geom2D.ControlPoint
import Flowbox.Geom2D.Path
import Flowbox.Geom2D.Shape
import Flowbox.Prelude

generateControls :: [Double] -> [ControlPoint Double]
generateControls = combine
    where combine [] = []
          combine (x1:y1:x2:y2:x3:y3:xs) = ControlPoint (Point2 x2 y2) (Point2 (x1-x2) (y1-y2)) (Point2 (x3-x2) (y3-y2)) : combine xs
          combine _ = error "unsupported ammount of coordinates"

generatePath :: [ControlPoint Double] -> Path Double
generatePath = flip Path True

main :: IO ()
main = do
    putStrLn "- - - = = =   Curves Test   = = = - - -"

    -- 600 x 500 image
    let coordinates = [
                      [ 64, 250, 64, 250, 34, 184
                      , 89, 150, 104, 147, 160, 134
                      , 184, 82, 230, 136, 281, 196
                      , 358, 173, 362, 187, 376, 236
                      , 295, 267, 293, 282, 286, 337
                      , 410, 404, 405, 418, 390, 460
                      , 329, 477, 301, 480, 286, 482
                      , 133, 454, 148, 449, 195, 433
                      , 181, 353, 176, 339, 161, 299
                      , 119, 256, 91, 310, 91, 310
                      ],
                      [ 386, 70, 386, 70, 369, 19
                      , 354, 51, 307, 43, 295, 35
                      , 217, 38, 236, 91, 221, 120
                      , 252, 122, 273, 159, 283, 196
                      , 310, 239, 257, 240, 212, 241
                      , 243, 318, 156, 299, 154, 319
                      , 160, 328, 193, 350, 213, 366
                      , 227, 369, 256, 357, 356, 363
                      , 429, 116, 489, 192, 561, 149
                      , 497, 126, 450, 106, 450, 106
                      ]
                      ]
        controls = fmap generateControls coordinates
        paths    = fmap generatePath     controls
        shape    = Shape paths

    print shape

    return ()
