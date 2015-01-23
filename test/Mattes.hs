---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import           Flowbox.Prelude
import           Flowbox.Graphics.Mockup as Mockup
import           Flowbox.Math.Matrix as M
import           Data.Array.Accelerate as A hiding (fromIntegral)
import           Data.Array.Accelerate.IO
import           Data.ByteString hiding (head)
import           Data.VectorSpace
--import           Diagrams.Prelude hiding (Path)
--import           Diagrams.Backend.Cairo
--import           Diagrams.Backend.Cairo.Internal
--import           Diagrams.Segment
--import           Diagrams.Trail
--import           Diagrams.TrailLike
import           Graphics.Rendering.Cairo hiding (translate)
--import           Graphics.Rendering.Cairo

import           Flowbox.Graphics.Image.Channel
import           Flowbox.Graphics.Shader.Shader
import           Flowbox.Geom2D.ControlPoint
import           Flowbox.Geom2D.Path
import           Flowbox.Graphics.Image.Matte
import           Flowbox.Geom2D.Mask as Mask
import           Flowbox.Geom2D.Rasterizer hiding (makePoints, makeSegments)
import           Flowbox.Graphics.Image.IO.BMP
import           Flowbox.Prelude hiding ((#))
import           Flowbox.Graphics.Color.RGBA as Color
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Math.Coordinate.Cartesian as Cartesian

circleMask :: DiscreteShader (A.Exp Double)
circleMask = 
    let
        f (Cartesian.Point2 x y) = (x*x + y*y <=* 50000) A.? (1.0,0.0)
    in
        Shader (Grid 1000 1000) f

main :: IO ()
main = do
    print "- - - = = =   Mattes Test  = = = - - -"

    let (w,h)  = (640, 480) :: (Int, Int)
    let closed = True
    let points = [ ControlPoint (Point2 212 209) Nothing                             (Just $ Point2 (211-212) (114-209))
                  , ControlPoint (Point2 338 210) (Just $ Point2 (329-338) (109-210)) (Just $ Point2 (450-338) (211-210))
                  , ControlPoint (Point2 343 330) (Just $ Point2 (456-343) (331-330)) Nothing
                  ]
    let feather = [ ControlPoint (Point2 212 (209-40)) Nothing                                (Just $ Point2 (211-212) (114-209))
                  , ControlPoint (Point2 338 (210-40)) (Just $ Point2 (329-338) (109-210)) (Just $ Point2 (450-338) (211-210))
                  , ControlPoint (Point2 343 (330-40)) (Just $ Point2 (456-343) (331-330)) Nothing
                  ]

    let pat = Path True points
    let fea = Path True feather
    let mask = (Mask.Mask pat (Just fea))
    let matte = VectorMatte mask

    let v1 = Color.RGBA 0.2 0.5 0.7 0.0
    let v2 = Color.RGBA 0.5 0.5 0.5 1.0
    let v3 = Color.RGBA 0.23 0.45 0.234 0.34
    let v4 = Color.RGBA 0.35 0.75 0.52 0.8
    let v5 = Color.RGBA 0.5 0.5 0.5 1.0
    let v6 = Color.RGBA 0.34 0.345 0.235 0.34
    let v7 = Color.RGBA 0.235 0.45 0.446 0.876
    let v8 = Color.RGBA 0.0 0.0 0.0 0.0
    let matte2 = ImageMatte $ ChannelFloat "matte" (DiscreteData circleMask)

    img <- loadImageLuna "lena.png"

    --let img1 = offsetMatteLuna v3 Nothing img
    --let img2 = contrastMatteLuna v1 Nothing img
    --let img3 = exposureMatteLuna v1 v5 Nothing img
    ----let img4 = gradeLuna' (VPS v1) (VPS v2) (VPS v2) v3 v5 v2 v2 matte img

    --let img1' = offsetMatteLuna v3 (Just matte) img
    --let img2' = contrastMatteLuna v1 (Just matte) img
    --let img3' = exposureMatteLuna v1 v5 (Just matte) img

    --let img1'' = offsetMatteLuna v3 (Just matte2) img
    --let img3'' = exposureMatteLuna v1 v5 (Just matte2) img

    ----let img4 = gradeLuna' (VPS v1) (VPS v2) (VPS v2) v3 v5 v2 v2 matte img

    --saveImageLuna "shape.png" (matrixToImage $ rasterizeMask w h $ (Mask.Mask pat (Just fea)))

    --saveImageLuna "lenaWithOffsetApplied.png" img1
    --saveImageLuna "lenaWithContrastApplied.png" img2
    --saveImageLuna "lenaWithExposureApplied.png" img3

    --saveImageLuna "lenaWithOffsetApplied-2.png" img1'
    --saveImageLuna "lenaWithContrastApplied-2.png" img2'
    --saveImageLuna "lenaWithExposureApplied-2.png" img3'  

    --saveImageLuna "lenaWithOffsetApplied-3.png" img1''
    --saveImageLuna "lenaWithExposureApplied-3.png" img3''

    ----saveImageLuna "lenaWithGradeApplied.png" img4

    print "done"
