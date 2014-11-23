---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Data.Array.Accelerate hiding (fromIntegral)
import           Data.Array.Accelerate.IO
import           Data.ByteString hiding (head)
import           Data.VectorSpace
import           Diagrams.Prelude
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Segment
import           Diagrams.TrailLike
import           Graphics.Rendering.Cairo hiding (translate)
--import           Graphics.Rendering.Cairo

import           Flowbox.Graphics.Image.IO.BMP
import           Flowbox.Prelude hiding ((#))



--def = FileOptions (400,400) PNG

makePoints h = combine
    where combine [] = []
          combine (x:y:xs) = (x,fromIntegral h - y) : combine xs
          combine _ = error "unsupported ammount of coordinates"

makeSegments = combine
    where combine [] = []
          combine [_] = []
          combine (o':c1':c2':x':xs) = let
                  o  = r2 o'
                  c1 = r2 c1'
                  c2 = r2 c2'
                  x  = r2 x'
              in bezier3 (c1 ^-^ o) (c2 ^-^ o) (x ^-^ o) : combine (x':xs)
          combine _ = error "unsupported ammount of points"

main = do
    let (w,h)  = (512, 512) :: (Int, Int)
        coords = [ 212, 209
                 , 211, 114, 329, 109, 338, 210
                 , 450, 211, 456, 331, 343, 330
                 --, 341, 447, 220, 445, 224, 334
                 ]
        points = makePoints h coords
        origin = head points
        segments = makeSegments points
        --myDiagram = (circle 10 # translate (r2 (50, 30)) :: Diagram B R2)
        --myDiagram = fromSegments
        --            [ bézier3 (r2 (211-212, 114-209)) (r2 (329-212, 109-209)) (r2 (338-212, 210-209))
        --            , bézier3 (r2 (450-338, 211-210)) (r2 (456-338, 331-210)) (r2 (343-338, 330-210))
        --            ]
        --             # translate (r2 (212,209))
        myDiagram = fromSegments segments # translate (r2 origin) # lw (Output 1.5) # lc white # bg white
        (_, r) = renderDia Cairo (CairoOptions "foo.png" (Width $ fromIntegral w) RenderOnly True) (myDiagram :: Diagram Cairo R2)

    surface <- createImageSurface FormatARGB32 w h
    renderWith surface r
    --surfaceWriteToPNG surface "foo.png"

    bs <- imageSurfaceGetData surface
    ar <- fromByteString (Z :. w :. h) ((), bs) :: IO (Array DIM2 Word32)

    --let img = makeRGBA ar

    return ()







