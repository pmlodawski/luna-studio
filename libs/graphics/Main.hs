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


import qualified Config              as Cfg
import           Control.Applicative
import qualified Data.Label          as Label
import qualified Monitoring          as Monitoring
import qualified ParseArgs           as ParseArgs
import qualified System.Environment  as Env
import qualified System.Exit         as Exit

import qualified Flowbox.Graphics.Raster.Image     as Image
import qualified Flowbox.Graphics.Raster.IO        as Image
import qualified Flowbox.Graphics.Raster.Repr.RGBA as RGBA
import           Flowbox.Prelude                   as P


--imgtest :: Image A.Word32 -> Either Image.Error (Image A.Word32)
imgtest img = do --imgFilter = do
    rgba  <- Image.reprFloat <$> RGBA.decompose img
    --rgbaFilter <- Image.reprFloat <$> RGBA.decompose imgFilter
    --lrgba <- adjustCB 2.2 0.2 "r" "g" "b" rgba
    --let blur3x3 = [0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1]
        --blur5x5 = [0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04]
        --sharpen3x3 = [-1.0,-1.0,-1.0,-1.0,9.0,-1.0,-1.0,-1.0,-1.0]
    --lrgba <- convolve "r" convolve5x5 sharpen3x3 rgba
    --hsv <- A.convertRGBtoHSV rgba
    --h <- Image.lookup "h" hsv
    --s <- Image.lookup "s" hsv
    --v <- Image.lookup "v" hsv
    --let hsvl = Image.insert "h" (Channel.map (mod1 . (+0.5)) h) $ hsv
    --let hsvl = Image.insert "s" (clipValues $ Channel.map (+1) s) $ hsv
    --lrgba <- blendRGB rgba rgbaFilter (blenderAlpha 0.5) -- ...
    --lrgba <- A.keyRGB 0.2 (0.055, 0.582, 0.363) rgba
    --lrgba <- A.luminance' rgba
    --        >>= Image.cpChannel "luminance" "r"
    --        >>= Image.cpChannel "luminance" "g"
    --        >>= Image.cpChannel "luminance" "b"
    --let f = \_ -> 1
    --    fBW = \x -> x A.>=* 0.5
    --    rgb = ("r", "g", "b")
    --lrgba <- A.keyRGB 0.1 (0.176, 0.816, 0.145) rgba
    --lrgba <- A.keyColor ("r", "g", "b") (0.2, 0.2, 0.2) (0.055, 0.582, 0.363) f rgba
    --lrgba <- A.keyColor ("r", "g", "b") (0.1, 0.1, 0.1) (0.176, 0.816, 0.145) f rgba
    --hsv2 <- A.keyColor ("h", "s", "v") (0.1, 0.2, 0.2) (0.402, 0.85, 0.59) f hsv
    --lrgba <- A.convertHSVtoRGB hsv2
    --bw <- A.binarizeImage rgb fBW lrgba
    --erodedBW <- A.erodeImage rgb bw
    --dilatedBW <- A.dilateImage rgb bw
    --erodedMono <- A.erodeImage rgb lrgba
    --dilatedMono <- A.dilateImage rgb lrgba
    --medianMono <- A.medianImage rgb lrgba
    --imgMedian <- A.medianImage rgb rgba
    RGBA.compose $ Image.reprWord8 rgba
    --where nonIntRem x y = x - (y * (A.fromIntegral $ (A.truncate (x / y) :: Exp Int)))
    --      mod1 = flip nonIntRem 1.0

-- main

main :: IO ()
main
  = do
        Monitoring.beginMonitoring

        argv                    <- Env.getArgs
        (conf, cconf, nops)     <- ParseArgs.parseArgs Cfg.configHelp Cfg.configBackend Cfg.options Cfg.defaults Cfg.header Cfg.footer argv
        (fileIn, fileOut)       <- case nops of
          (i:o:_) -> return (i,o)
          _       -> ParseArgs.parseArgs Cfg.configHelp Cfg.configBackend Cfg.options Cfg.defaults Cfg.header Cfg.footer ("--help":argv)
                  >> Exit.exitSuccess

        let backend     = Label.get Cfg.configBackend conf
        -- Read in the image file

        img2 <- either (\_ -> mempty) id `fmap` Image.readImageFromBMP fileIn
        --imgFilter <- either (\_ -> mempty) id `fmap` Image.readImageFromBMP "filter.bmp"
        let img3 = imgtest img2 --imgFilter

        case img3 of
            Left  err -> print err
            Right val -> do Image.writeImageToBMP (ParseArgs.run backend) fileOut val
                            return ()


        --if P.not (Label.get Cfg.configBenchmark conf)
        --   then do
        --     -- Connect the strong and weak edges of the image using Repa, and
        --     -- write the final image to file
        --     --
        --     --let (image, strong) = ParseArgs.run backend $ A.lift (Canny.canny threshLow threshHigh (A.use img))
        --     --edges              <- Wildfire.wildfire (A.toRepa image) (A.toRepa strong)
        --     --R.writeImageToBMP fileOut (R.zip3 edges edges edges)


        --         --print "!!!1"
        --         ---- Connect the strong and weak edges of the image using Repa, and
        --         ---- write the final image to file
        --         ----
        --         --print "1"
        --         --let (image, strong) = ParseArgs.run backend $ A.lift (Canny.canny threshLow threshHigh (A.use img))
        --         --print "2"
        --     --let test            = A.toRepa ltest -- :: Int -- R.Array R.U R.DIM2 Word8
        --     --    ----print (test `R.deepSeq` "#2")
        --     --    --print "3"
        --     --test2 <- timeIt (demote2 test :: IO (R.Array R.U R.DIM2 Word8))
        --         --print "4"
        --         ----edges              <- Wildfire.wildfire (A.toRepa image) (A.toRepa strong)
        --         --print "write"
        --     --timeIt $ R.writeImageToBMP fileOut (R.zip3 test2 test2 test2)
        --     return ()

        --  else do
        --    -- Run each of the individual kernel stages through criterion, as
        --    -- well as the end-to-end step process.
        --    --
        --    Env.withArgs (P.drop 2 nops) $ defaultMainWith cconf (return ())
        --      [ bgroup "kernels"
        --        [   --  bench "normalize"   $ whnf normalizeRGBA32 img
        --            --, bench "demote"      $ (demote test :: IO (R.Array R.U R.DIM2 Word8))
        --            --, bench "file read"   $ either (error . show) id `fmap` A.readImageFromBMP fileIn
        --            --, bench "greyscale"   $ whnf ((ParseArgs.run backend) . Canny.toGreyscale) (A.use img)
        --            --, bench "cos"         $ whnf (\img' -> ParseArgs.run backend $ A.lift (Canny.canny threshLow threshHigh (A.use img'))) img
        --            --, bench "toRepa"      $ whnf (A.toRepa) grey'
        --            --, bench "write"       $ R.writeImageToBMP fileOut (R.zip3 test2 test2 test2)
        --        ]

        --      ]

        --    --Env.withArgs (P.drop 2 nops) $ defaultMainWith cconf (return ())
        --    --  [ bgroup "kernels"
        --    --    [ bench "greyscale"   $ whnf (ParseArgs.run1 backend Canny.toGreyscale) img
        --    --    , bench "blur-x"      $ whnf (ParseArgs.run1 backend Canny.gaussianX) grey'
        --    --    , bench "blur-y"      $ whnf (ParseArgs.run1 backend Canny.gaussianY) blurX'
        --    --    , bench "grad-x"      $ whnf (ParseArgs.run1 backend Canny.gradientX) blurred'
        --    --    , bench "grad-y"      $ whnf (ParseArgs.run1 backend Canny.gradientY) blurred'
        --    --    , bench "mag-orient"  $ whnf (ParseArgs.run1 backend (Canny.gradientMagDir low)) blurred'
        --    --    , bench "suppress"    $ whnf (ParseArgs.run1 backend (Canny.nonMaximumSuppression low high)) magdir'
        --    --    , bench "select"      $ whnf (ParseArgs.run1 backend Canny.selectStrong) suppress'
        --    --    ]

        --    --  , bgroup "canny"
        --    --    [ bench "run"     $ whnf (ParseArgs.run backend . (P.snd . Canny.canny threshLow threshHigh)) (A.use img)
        --    --    , bench "run1"    $ whnf (ParseArgs.run1 backend  (P.snd . Canny.canny threshLow threshHigh)) img
        --    --    ]
        --    --  ]

