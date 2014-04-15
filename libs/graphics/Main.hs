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
import           Control.Applicative
import           Data.Array.Accelerate             (Exp)
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Interpreter as Interp
import qualified Data.Map                          as Map
import qualified Monitoring         as Monitoring
import qualified ParseArgs          as ParseArgs
import qualified System.Environment as Env
import qualified System.Exit        as Exit

import           Diagrams.Prelude            as Diag
import           Diagrams.TwoD.Path.Metafont

--import qualified Debug.Trace           as Dbg

import           Flowbox.Graphics.Color                 (Color (..))
import qualified Flowbox.Graphics.Color                 as C
import qualified Flowbox.Graphics.Deprecated.Algorithms as G
import           Flowbox.Graphics.Image                 (ImageAcc)
import qualified Flowbox.Graphics.Image                 as Img
import           Flowbox.Graphics.Image.Channel         (Select(..))
import qualified Flowbox.Graphics.Image.Channel         as Channel
import           Flowbox.Graphics.Image.Composition     (Mask (..), Premultiply (..))
import qualified Flowbox.Graphics.Image.Composition     as Comp
import qualified Flowbox.Graphics.Image.Color           as Img
import qualified Flowbox.Graphics.Image.IO              as Img
import qualified Flowbox.Graphics.Image.Raster          as Img
import qualified Flowbox.Graphics.Image.Repr            as Repr
import qualified Flowbox.Graphics.Shape                 as Shape
import           Flowbox.Graphics.Utils                 (Range(..))
import qualified Flowbox.Graphics.Utils                 as U
import           Flowbox.Prelude                        as P

import Control.Lens as L
--diag = square 1 Diag.# fcA (goldenrod `withOpacity` 0.5)
z1 = p2 (0,1)
z2 = p2 (1,1)
z3 = p2 (2,1)
z4 = p2 (0,0)
z5 = p2 (1,0)
z6 = p2 (2,0)
meta = metafont $ z4.--.z1.--.z2.--.z6.- tension 1.5 -.cyclePath
--color = (goldenrod `withOpacity` 0.5)
color = white `withOpacity` 1
diag = (strokeLoop meta) Diag.# fcA color

imgtest img frames mask = do
    let getDouble image = Img.toDouble <$> Repr.decompose image
        red       = RGB (A.constant 1) (A.constant 0) (A.constant 0)
        green     = RGB (A.constant 0) (A.constant 1) (A.constant 0)
        blue      = RGB (A.constant 0) (A.constant 0) (A.constant 1)
        white     = RGB (A.constant 1) (A.constant 1) (A.constant 1)
        black     = RGB (A.constant 0) (A.constant 0) (A.constant 0)
        gray      = RGB (A.constant 0.5) (A.constant 0.5) (A.constant 0.5)
        yellow    = RGB (A.constant 1) (A.constant 1) (A.constant 0)
        gammaMap  = Map.fromList [ ("rgba.r", 0.5), ("rgba.g", 0.5), ("rgba.b", 0.5) ]
        clampMap  = Map.fromList [ ("rgba.r", (Range 0.25 0.75, Just (Range 1 0)))
                                 , ("rgba.g", (Range 0.25 0.75, Just (Range 0 1)))
                                 , ("rgba.b", (Range 0.25 0.75, Just (Range 0 0))) ]
        clipMap   = Map.fromList [ ("rgba.r", Range 0.19 1)
                                 , ("rgba.g", Range 0.19 1)
                                 , ("rgba.b", Range 0.19 1) ]
        selection = ChannelList  [ "rgba.r", "rgba.g", "rgba.b" ]

    imageRGBA <- getDouble img
    framesRGBA <- getDouble frames
    maskDouble <- getDouble mask

    --imageBackground <- G.extractBackground rgb framesRGBA
    let imageConstant     = Img.constant (A.index2 (512::Exp Int) (512::Exp Int)) [("rgba.r", A.constant 1), ("rgba.g", A.constant 0), ("rgba.b", A.constant 1), ("rgba.a", A.constant 1)]
        imageCheckerboard = Img.checkerboard (A.index2 (512::Exp Int) (512::Exp Int)) (A.constant 32) (black, white, black, white) (red, A.constant 0) (yellow, A.constant 0)
        --imageMask         = ImageMask "rgba.r" Nothing (A.constant False) imageCheckerboard
        imageMask         = ImageMask "rgba.r" Nothing (A.constant False) maskDouble

    maskChannel <- Img.get "rgba.r" imageCheckerboard

    let imageRGBAwithMask = Img.insert "mask.a" maskChannel imageRGBA
        premultiply = Premultiply "mask.a" (A.constant False)

    imageGamma    <- Img.gamma    imageRGBAwithMask gammaMap (Just imageMask) Nothing 1
    imageClamp    <- Img.clamp    imageRGBAwithMask clampMap Nothing Nothing 1
    imageClipTest <- Img.clipTest imageRGBAwithMask clipMap (Just imageMask) (Just premultiply) 1
    imageInvert   <- Img.invert   imageRGBAwithMask selection Nothing Nothing Nothing 1

    let imageOut = imageGamma

    Repr.compose $ Img.toWord8 $ Img.map G.clipValues imageOut

---- main

main :: IO ()
main
  = do
        Monitoring.beginMonitoring

        argv                    <- Env.getArgs
        (conf, nops)            <- ParseArgs.parseArgs Cfg.configHelp Cfg.configBackend Cfg.options Cfg.defaults Cfg.header Cfg.footer argv
        (fileIn, fileOut)       <- case nops of
          (i:o:_) -> return (i,o)
          _       -> ParseArgs.parseArgs Cfg.configHelp Cfg.configBackend Cfg.options Cfg.defaults Cfg.header Cfg.footer ("--help":argv)
                  >> Exit.exitSuccess

        let backend     = ParseArgs.Interpreter --Label.get Cfg.configBackend conf
            frameNames  = replicate 5 "lena.bmp"
            --frameNames  = fmap (\x -> (T.printf "frames/frame-small-%03d.bmp" x) :: String) ([1,5..66] :: [Int])
            getImage location = fmap (either (\_ -> mempty) id) (Img.readFromBMP location)
            getImages locations = fmap (either (\_ -> mempty) id) (Img.readSequenceFromBMP locations)
            getDouble image = Img.toFloat <$> Repr.decompose image

        -- Read in the image file
        imageIn <- getImage fileIn
        framesIn <- getImages frameNames

        diagram       <- Shape.rasterize 512 512 100 100 (Diag.Width 256) diag
        let dupa =  Img.toWord8 $ Img.map G.clipValues diagram

        let rasterizedDiagram = case Repr.compose dupa of
                Left err  -> mempty
                Right val -> val
            --imageOut = Right rasterizedDiagram

        let imageOut = imgtest imageIn framesIn rasterizedDiagram

        case imageOut of
            Left err  -> print err
            Right val -> do Img.writeToBMP (ParseArgs.run backend) fileOut val
                            return ()


        -- COLOR TESTS

        let x = 0.5 :: A.Exp Double
            y = 0.3 :: A.Exp Double
            z = 0.2 :: A.Exp Double
            rgb  = RGB x y z
            rgba = C.toRGBA rgb
            hsv  = C.toHSV rgba
            hsl  = C.toHSL hsv
            cmy  = C.toCMY hsl
            cmyk = C.toCMYK cmy
            rgb' = C.toRGB cmyk
            (RGB r g b) = rgb'

        print $ Interp.run $ A.unit $ A.lift (r, g, b)


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

