---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Graphics.Mockup (
    module Flowbox.Graphics.Mockup,
    Image,
    Image.reprDouble,
    Image.reprWord8,
    Channel,
    Image.Transformed,

    Alg.invert,
    Alg.invert',
    Alg.sign,
    Alg.parametrize,
    Alg.bias,
    Alg.gain,
    Alg.gamma,
    Alg.compress,
    Alg.expand,
    Alg.remap,

    Alg.erodeChannel,
    toDouble,
    Exp,

) where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A
import           GHC.Float
import qualified System.Exit           as Exit
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA as CUDA
#endif
import qualified Data.Array.Accelerate.Interpreter as Interpreter

import qualified Data.Map                          as Map
import           Data.Number.Conversion
import qualified Flowbox.Graphics.Algorithms       as Alg
import           Flowbox.Graphics.Raster.Channel   (Channel)
import qualified Flowbox.Graphics.Raster.Channel   as Channel
import           Flowbox.Graphics.Raster.Image     (Image (Image))
import qualified Flowbox.Graphics.Raster.Image     as Image
import qualified Flowbox.Graphics.Raster.IO        as Image
import qualified Flowbox.Graphics.Raster.Repr.RGBA as RGBA
import           Flowbox.Prelude                   hiding ((.))
import           Luna.Target.HS.Core               hiding (print, return)



-- Backends --------------------------------------------------------------
runBackend :: A.Elt a => LunaBackend -> Channel.Backend a
#ifdef ACCELERATE_CUDA_BACKEND
runBackend LunaCUDA = CUDA.run
#endif
runBackend LunaInterpreter = Interpreter.run


data LunaBackend = LunaCUDA
                 | LunaInterpreter
                 deriving(Show)


cuda :: LunaBackend
cuda = LunaCUDA


interp :: LunaBackend
interp = LunaInterpreter

-- Image -----------------------------------------------------------------

-- FIXME[wd]: UNSAFE ERROR
writeImage :: Image (A.Word32) -> FilePath -> LunaBackend -> IO (Safe ())
writeImage img path backend = do
    Image.writeImageToBMP (runBackend backend) path img
    return (Safe ())

adjustCB :: Double -> Double -> Image Double -> Pure (Either Image.Error (Image Double))
adjustCB contrastValue brightnessValue img =
    Pure $ Alg.adjustCB_RGB (A.constant contrastValue) (A.constant brightnessValue) img


convolve :: Double -> Image Double -> Pure (Either Image.Error (Image Double))
convolve kernel img = Pure $ Alg.convolveRGB Alg.convolve3x3 kernel' img where
    kernel' = map A.constant $ replicate 9 kernel


rasterize' :: (A.Elt a, A.IsFloating a, Functor m, Functor n) =>
              Image.Transformed (m ( n( Image a))) -> m (n (Image a))
rasterize' (Image.Transformed img t) =
    (fmap.fmap) (\i -> Image $ Map.map (Image.rasterizeChannel t) $ view Image.channels i) img
