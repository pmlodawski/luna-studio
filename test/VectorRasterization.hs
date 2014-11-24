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
import           Diagrams.Trail
import           Diagrams.TrailLike
import           Graphics.Rendering.Cairo hiding (translate)
--import           Graphics.Rendering.Cairo

import           Flowbox.Geom2D.Rasterizer hiding (makePoints, makeSegments)
import           Flowbox.Graphics.Image.IO.BMP
import           Flowbox.Graphics.Mockup (saveImageLuna)
import           Flowbox.Prelude hiding ((#))



--def = FileOptions (400,400) PNG

makePoints = combine
    where combine [] = []
          combine (x:y:xs) = (x,y) : combine xs
          combine _ = error "unsupported ammount of coordinates"

makeSegments = combine
    where combine [] = []
          combine [_] = []
          combine (a':b':c':d':xs) = let
                  a = r2 a'
                  b = r2 b'
                  c = r2 c'
                  d = r2 d'
            in bezier3 (b ^-^ a) (c ^-^ a) (d ^-^ a) : combine (d':xs)
          combine _ = error "unsupported ammount of points"

main = do
    let closed = True
        (w,h)  = (512, 512) :: (Int, Int)
        --coords = [ 212, 209
        --         , 211, 114, 329, 109, 338, 210
        --         , 450, 211, 456, 331, 343, 330
        --         --, 341, 447, 220, 445, 224, 334
        --         ]
        coords = [ 0, 0
                 , 100, 0, 100, 100, 0, 100
                 ]
        --points' = [ Point2 212 209
        --          , Point2 211 114, Point2 329 109, Point2 338 210
        --          , Point2 450 211, Point2 456 331, Point2 343 330
        --          ]
        points' = [ Point2 0 0
                  , Point2 100 0, Point2 100 100, Point2 0 100
                  ]
        points = makePoints coords
        origin = head points
        segments = makeSegments points
        path = fromSegments segments
        myDiagram = case closed of
            False -> path # translate (r2 origin) # lw (Output 1) # lc white
            True -> (strokeLoop.closeLine) path # translate (r2 origin) # fc white
        (_, r) = renderDia Cairo (CairoOptions "foo.png" (Dims (fromIntegral w) (fromIntegral h)) RenderOnly True) (myDiagram :: Diagram Cairo R2)

    --print "TEST:"
    --print myDiagram
    surface <- createImageSurface FormatARGB32 w h
    renderWith surface r
    --surfaceWriteToPNG surface "foo.png"

    bs <- imageSurfaceGetData surface
    ar <- fromByteString (Z :. w :. h) ((), bs) :: IO (Array DIM2 Word32)

    writeImageToBMP "foo.bmp" ar

    let img = rasterizeVector w h closed points'
    saveImageLuna "foo2.png" img

    return ()







