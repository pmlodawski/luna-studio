{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Graphics.Deprecated.Rendering.Cairo (
    module Flowbox.Graphics.Deprecated.Rendering.Cairo,
    module X
) where


import qualified System.IO.Unsafe         as Unsafe

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.Rendering.Cairo as X hiding ( createImageSurface
                                                          , surfaceSetDeviceOffset
                                                          , renderWith
                                                          , imageSurfaceGetPixels
                                                          )
import qualified Data.Array.MArray        as MA
import Data.Function


createImageSurface format w h = Unsafe.unsafePerformIO $ Cairo.createImageSurface format w h

surfaceSetDeviceOffset surface x y = Unsafe.unsafePerformIO $ Cairo.surfaceSetDeviceOffset surface x y

renderWith surface render = Unsafe.unsafePerformIO $ Cairo.renderWith surface render

imageSurfaceGetPixels = Unsafe.unsafePerformIO . Cairo.imageSurfaceGetPixels

getElems = Unsafe.unsafePerformIO . MA.getElems
