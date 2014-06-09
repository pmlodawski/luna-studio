---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeOperators        #-}

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExtendedDefaultRules      #-}


import qualified Config                            as Cfg
import qualified Monitoring         as Monitoring
import qualified ParseArgs          as ParseArgs
import qualified System.Environment as Env
import qualified System.Exit        as Exit

--import qualified Debug.Trace           as Dbg

import Data.Array.Accelerate as A

import           Flowbox.Data.Channel                 as DC
import           Flowbox.Math.Matrix                  as M
import           Flowbox.Graphics.Image.Channel       as C
import           Flowbox.Graphics.Image.View          as V
import           Flowbox.Prelude                      as P

--imgtest img = do
--    let getDouble image = Img.toDouble <$> Repr.decompose image
--        --Right mask2     = Repr.compose $ Img.toWord8 $ Img.map G.clipValues $ Unsafe.unsafePerformIO $ Shape.rasterize 512 512 100 100 (Diag.Width 256) diag
--        --Right mask2     = Repr.compose $ Img.toWord8 $ Img.map G.clipValues $ DShape.rasterize 512 512 100 100 (Diag.Width 256) diag
--        --red       = RGB (A.constant 1) (A.constant 0) (A.constant 0)
--        --green     = RGB (A.constant 0) (A.constant 1) (A.constant 0)
--        --blue      = RGB (A.constant 0) (A.constant 0) (A.constant 1)
--        --white     = RGB (A.constant 1) (A.constant 1) (A.constant 1)
--        --black     = RGB (A.constant 0) (A.constant 0) (A.constant 0)
--        --gray      = RGB (A.constant 0.5) (A.constant 0.5) (A.constant 0.5)
--        --yellow    = RGB (A.constant 1) (A.constant 1) (A.constant 0)
--        --gammaMap  = Map.fromList [ ("rgba.r", 2.5), ("rgba.g", 2.5), ("rgba.b", 2.5) ]
--        --contrastMap = Map.fromList [ ("rgba.r", 2), ("rgba.g", 2), ("rgba.b", 2) ]
--        --clampMap  = Map.fromList [ ("rgba.r", (Range 0.25 0.75, Just (Range 1 0)))
--        --                         , ("rgba.g", (Range 0.25 0.75, Just (Range 0 1)))
--        --                         , ("rgba.b", (Range 0.25 0.75, Just (Range 0 0))) ]
--        --clipMap   = Map.fromList [ ("rgba.r", Range 0.19 1)
--        --                         , ("rgba.g", Range 0.19 1)
--        --                         , ("rgba.b", Range 0.19 1) ]
--        --selection = ChannelList  [ "rgba.r", "rgba.g", "rgba.b" ]
--        --channelsOut = [ "rgba.r", "rgba.g" ]
--        --channelsIn  = [ "rgba.r", "rgba.g", "rgba.b" ]
--        --colorMatrix' = [ 0,   0.8,   0
--        --               , 0.5, 0.5, 0.5 ]
--        --colorMatrix = A.use $ (A.fromList (A.Z A.:. 2 A.:. 3) colorMatrix' :: A.Array A.DIM2 Double)

--    imageRGBA  <- getDouble img
--    --framesRGBA <- getDouble frames
--    --maskDouble <- getDouble mask2
--    --sourceRGBA <- getDouble source
--    --targetRGBA <- getDouble target

--    --imageBackground <- G.extractBackground rgb framesRGBA
--    --let imageConstant     = Img.constant (A.index2 (512::Exp Int) (512::Exp Int)) [("rgba.r", A.constant 1), ("rgba.g", A.constant 0), ("rgba.b", A.constant 1), ("rgba.a", A.constant 1)]
--    --    imageCheckerboard = Img.checkerboard (A.index2 (512::Exp Int) (512::Exp Int)) (A.constant 32) (black, white, black, white) (red, A.constant 0) (yellow, A.constant 0)
--    --    --imageMask         = Mask "rgba.r" Nothing (A.constant False) imageCheckerboard
--    --    imageMask         = Mask "rgba.r" Nothing (A.constant False) maskDouble

--    --maskChannel <- Img.get "rgba.r" imageCheckerboard

--    --let imageRGBAwithMask = Img.insert "mask.a" maskChannel imageRGBA
--    --    premultiply = Premultiply "mask.a" (A.constant False)

--    --imageGamma       <- Img.gamma    imageRGBA gammaMap Nothing Nothing 1
--    --imageClamp       <- Img.clamp    imageRGBAwithMask clampMap Nothing Nothing 1
--    --imageClipTest    <- Img.clipTest imageRGBAwithMask clipMap (Just imageMask) (Just premultiply) 1
--    --imageInvert      <- Img.invert   imageRGBAwithMask selection Nothing (Just imageMask) Nothing 1
--    --imageColorMatrix <- Img.colorMatrix imageRGBAwithMask channelsOut channelsIn colorMatrix (Just imageMask) Nothing 1
--    --imageContrast    <- Img.contrast imageRGBA contrastMap Nothing Nothing 1
--    --imageTransfered  <- Img.colorTransfer targetRGBA sourceRGBA Nothing Nothing 1

--    let imageOut = imageRGBA

--    Repr.compose $ Img.toWord8 $ Img.map G.clipValues imageOut

---- main

main :: IO ()
main
  = do
        Monitoring.beginMonitoring

        argv                    <- Env.getArgs
        (conf, nops)            <- ParseArgs.parseArgs Cfg.configHelp Cfg.configBackend Cfg.options Cfg.defaults Cfg.header Cfg.footer argv
        (fileIn, fileOut, rest)       <- case nops of
          (i:o:rest) -> return (i,o,rest)
          _       -> ParseArgs.parseArgs Cfg.configHelp Cfg.configBackend Cfg.options Cfg.defaults Cfg.header Cfg.footer ("--help":argv)
                  >> Exit.exitSuccess

        let noCUDA = case rest of
                (opt:_) -> opt == "no-cuda"
                _       -> False

        --let backend     = ParseArgs.Interpreter --Label.get Cfg.configBackend conf
        --    backendRun  = case noCUDA of
        --        False -> CUDA.run
        --        True  -> Interp.run
        --    frameNames  = replicate 5 "lena.bmp"
        --    --frameNames  = fmap (\x -> (T.printf "frames/frame-small-%03d.bmp" x) :: String) ([1,5..66] :: [Int])
        --    getImage location = fmap (either (\_ -> mempty) id) (Img.readFromBMP location)
        --    getImages locations = fmap (either (\_ -> mempty) id) (Img.readSequenceFromBMP locations)
        --    getDouble image = Img.toFloat <$> Repr.decompose image

        ---- Read in the image file
        --imageIn <- getImage fileIn
        ----framesIn <- getImages frameNames

        ----sourceImage <- getImage "sea-picture.bmp"
        ----targetImage <- getImage "sea-rendered.bmp"

        ----diagram       <- DShape.rasterize 512 512 100 100 (Diag.Width 256) diag
        ----let diagram = DShape.rasterize 512 512 100 100 (Diag.Width 256) diag
        ----let dupa =  Img.toWord8 $ Img.map G.clipValues diagram

        ----let rasterizedDiagram = case Repr.compose dupa of
        ----        Left err  -> mempty
        ----        Right val -> val
        --    --imageOut = Right rasterizedDiagram

        --let imageOut = imgtest imageIn

        --case imageOut of
        --    Left err  -> print err
        --    --Right val -> do Img.writeToBMP (ParseArgs.run backend) fileOut val
        --    Right val -> do Img.writeToBMP (backendRun) fileOut val
        --                    return ()

        let testArray = A.use $ A.fromList (Z :. 2 :. 2) [1,2,3,4] :: Acc (Array DIM2 Int)
            testMatrix = Delayed testArray
            testChanData = FlatData testMatrix
            testChan = ChannelInt "r" testChanData
            --testView = V.empty RGB "rgbImageMadafaka"
            --testView' = V.append "layer1" Nothing testView >>= V.append "layer1.rgb" Nothing >>= V.append "layer1.rgb.r" (Just testChan)

        --print testView'

        return ()

        -- BEZIER TESTS

        --let p0 = Point 30 70
        --    p1 = Point 0 270
        --    p2 = Point 290 110
        --    p3 = Point 200 100
        --    curve = CubicBezier p0 p1 p2 p3
        --    curvePath = OpenPath [(p0, JoinCurve p1 p2)] p3
        --    bb = Shape.bezierBoundingBox curve 0.001
        --    curveRaster = Shape.rasterizeMask 512 512 10 10 (Shape.Width 380) curvePath
        --    Right alpha = Img.get "rgba.a" curveRaster
        --    curveRaster' = Img.insert "rgba.r" alpha
        --                 $ Img.insert "rgba.g" alpha
        --                 $ Img.insert "rgba.b" alpha
        --                 $ curveRaster
        --    curveOut = Repr.compose $ Img.toWord8 $ Img.map G.clipValues curveRaster'

        --print bb

        --case curveOut of
        --    Left err  -> print err
        --    Right val -> do Img.writeToBMP (backendRun) "curve.bmp" val
        --                    return ()


        --print $ Interp.run $ A.unit $ A.lift (r, g, b)


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

