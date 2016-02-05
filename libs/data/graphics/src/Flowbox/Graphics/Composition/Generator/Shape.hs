---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Composition.Generator.Shape where


import           Flowbox.Graphics.Shader.Shader
import           Flowbox.Math.Index             hiding (canvas)
import           Flowbox.Prelude                hiding (transform)

import           Data.Array.Accelerate          as A hiding (constant)
import           Math.Coordinate.Cartesian      hiding (w, x, y)
import           Math.Space.Space



constant :: Grid (Exp Int) -> b -> Shader a b
constant cnv a = Shader cnv $ const a

rectangle :: Elt b => Grid (Exp Int) -> Exp b -> Exp b -> DiscreteShader (Exp b)
rectangle cnv interior exterior = bound (Constant exterior) $ constant cnv interior

ellipse :: Elt b => Grid (Exp Int) -> Exp b -> Exp b -> DiscreteShader (Exp b)
ellipse cnv@(Grid w h) interior exterior = Shader cnv $ \(Point2 x y) ->
    let rx = (w `div` 2)
        rx2 = rx * rx
        ry = (h `div` 2)
        ry2 = ry * ry
        x2 = (x - rx) * (x - rx)
        y2 = (y - ry) * (y - ry)
    in A.cond ((x2 * ry2 + y2 * rx2) A.<=* (rx2 * ry2)) interior exterior
