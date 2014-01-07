{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Applicative

import qualified Canny      as Canny
import qualified Config     as Cfg
import qualified Monitoring as Monitoring
import qualified ParseArgs  as ParseArgs
import qualified Wildfire   as Wildfire

import           Criterion.Main     (bench, bgroup, defaultMainWith, whnf)
import qualified Data.Label         as Label
import           Flowbox.Prelude    as P
import qualified System.Environment as Env
import qualified System.Exit        as Exit

import           Data.Array.Accelerate        ((:.) (..), Acc, Exp)
import qualified Data.Array.Accelerate        as A
import qualified Data.Array.Accelerate.IO     as A
import qualified Data.Array.Repa              as R
import qualified Data.Array.Repa.IO.BMP       as R
import qualified Data.Array.Repa.IO.DevIL     as DevIL
import qualified Data.Array.Repa.Repr.Unboxed as R
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Monoid                  (Monoid, mempty)
import qualified Data.Fixed                   as F

import System.TimeIt (timeIt)

import Data.Array.Repa.Eval (Target)
import Data.Word            (Word8)

import           Flowbox.Graphics.Raster.Channel   (Channel)
import qualified Flowbox.Graphics.Raster.Channel   as Channel
import           Flowbox.Graphics.Raster.Image     (Image)
import qualified Flowbox.Graphics.Raster.Image     as Image
import qualified Flowbox.Graphics.Raster.IO        as Image
import qualified Flowbox.Graphics.Raster.Repr.RGBA as RGBA

--import           Control.Monad


import qualified Data.Array.Repa.Eval as R

import Data.Bits ((.&.))


--import qualified Data.Array.Accelerate.CUDA             as CUDA

import Control.Monad.Trans.Either (hoistEither, runEitherT)



luminance :: String -> String -> String -> String -> (Image Float) -> Either Image.Error (Image Float)
luminance rname gname bname outname img = do
    chr <- Image.lookup rname img
    chg <- Image.lookup gname img
    chb <- Image.lookup bname img
    let chan = Channel.zipWith3 colormix chr chg chb
        colormix r g b = 0.3 * r + 0.59 * g + 0.11 * b
        outimg = Image.insert outname chan img
    return outimg

luminance' :: (Image Float) -> Either Image.Error (Image Float)
luminance' = luminance "r" "g" "b" "luminance"



imgtest :: Image A.Word32 -> Either Image.Error (Image A.Word32)
imgtest img = do
    rgba  <- Image.reprFloat <$> RGBA.decompose img
    lrgba <- luminance' rgba
           >>= Image.cpChannel "luminance" "r"
           >>= Image.cpChannel "luminance" "g"
           >>= Image.cpChannel "luminance" "b"
    RGBA.compose $ Image.reprWord8 lrgba



-- convolution

convolve3x3 :: (A.Elt a, A.IsNum a) => [A.Exp a] -> A.Stencil3x3 a -> A.Exp a
convolve3x3 kernel ((a,b,c),(d,e,f),(g,h,i))
    = P.sum $ P.zipWith (*) kernel [a,b,c,d,e,f,g,h,i]

-- ?!?!?!?!?!?!?!?!
blurChannel :: String -> [A.Exp Float] -> (Image Float) -> Either Image.Error (Image Float)
blurChannel cname kernel img = do
    channel <- Image.lookup cname img
    let outimg = Image.insert cname channel' img
        channel' = Channel.Acc $ A.stencil (convolve3x3 kernel) A.Clamp (Channel.accMatrix channel)
    return outimg



-- brightness and contrast

adjustCB :: A.Exp Float -> A.Exp Float -> String -> (Image Float) -> Either Image.Error (Image Float)
adjustCB contrast brightness cname img = do
    channel <- Image.lookup cname img
    let outimg = Image.insert cname channel' img
        channel' = Channel.Acc $ A.map adjust (Channel.accMatrix channel)
        adjust x = contrast * x + brightness
    return outimg

contrast :: A.Exp Float -> String -> (Image Float) -> Either Image.Error (Image Float)
contrast c = adjustCB c 0

brightness :: A.Exp Float -> String -> (Image Float) -> Either Image.Error (Image Float)
brightness b = adjustCB 1 b



-- color conversion

clipValues :: (Channel Float) -> (Channel Float)
clipValues channel = Channel.Acc $ A.map (P.max 0 . P.min 1) $ Channel.accMatrix channel

calculateHueFromRGB :: Exp Float -> Exp Float -> Exp Float -> Exp Float
calculateHueFromRGB r g b = ((60 * (h - d / (maxRGB - minRGB))) `F.mod'` 360) / 360
    where h      = if r == minRGB then 3 else (if b == minRGB then 1 else 5)
          d      = if r == minRGB then g-b else (if b == minRGB then r-g else b-r)
          minRGB = P.min r $ P.min g b
          maxRGB = P.max r $ P.max g b

calculateSaturationFromRGB :: Exp Float -> Exp Float -> Exp Float -> Exp Float
calculateSaturationFromRGB r g b = (maxRGB - minRGB) / maxRGB
    where maxRGB = P.max r $ P.max g b
          minRGB = P.min r $ P.min g b

calculateValueFromRGB :: Exp Float -> Exp Float -> Exp Float -> Exp Float
calculateValueFromRGB r g b = P.max r $ P.max g b

convertRGBtoHSV :: (Image Float) -> Either Image.Error (Image Float)
convertRGBtoHSV img = do
    r <- Image.lookup "red"   img
    g <- Image.lookup "green" img
    b <- Image.lookup "blue"  img
    let outimg     = Image.insert "hue" hue
                   $ Image.insert "saturation" saturation
                   $ Image.insert "value" value
                   $ img
        hue        = Channel.Acc $ A.zipWith3 calculateHueFromRGB   r' g' b'
        saturation = Channel.Acc $ A.zipWith3 calculateValueFromRGB r' g' b'
        value      = Channel.Acc $ A.zipWith3 calculateValueFromRGB r' g' b'
        r'         = Channel.accMatrix r
        g'         = Channel.accMatrix g
        b'         = Channel.accMatrix b
    return outimg

calculateRGBfromHSV :: Exp Float -> Exp Float -> Exp Float -> (Exp Float, Exp Float, Exp Float)
calculateRGBfromHSV h s v = (r+m, g+m, b+m)
    where (r, g, b) = case h' of
                      z | 0   <= z && z < 60  -> (c, x, 0)
                      z | 60  <= z && z < 120 -> (x, c, 0)
                      z | 120 <= z && z < 180 -> (0, c, x)
                      z | 180 <= z && z < 240 -> (0, x, c)
                      z | 240 <= z && z < 300 -> (x, 0, c)
                      z | 300 <= z && z < 360 -> (c, 0, x)
          m  = v - c
          x  = c * (1 - P.abs (((h' / 60) `F.mod'` 2) - 1))
          c  = v * s
          h' = (h `F.mod'` 1) * 360

convertHSVtoRGB :: (Image Float) -> Either Image.Error (Image Float)
convertHSVtoRGB img = do
    h <- Image.lookup "hue" img
    s <- Image.lookup "saturation" img
    v <- Image.lookup "value" img
    let outimg = Image.insert "red" red
               $ Image.insert "green" green
               $ Image.insert "blue" blue
               $ img
        red    = Channel.Acc $ A.zipWith3 calcR h' s' v'
        green  = Channel.Acc $ A.zipWith3 calcG h' s' v'
        blue   = Channel.Acc $ A.zipWith3 calcB h' s' v'
        calcR  = (\a b c -> let (z, _, _) = calculateRGBfromHSV a b c in z)
        calcG  = (\a b c -> let (_, z, _) = calculateRGBfromHSV a b c in z)
        calcB  = (\a b c -> let (_, _, z) = calculateRGBfromHSV a b c in z)
        h' = Channel.accMatrix h
        s' = Channel.accMatrix s
        v' = Channel.accMatrix v
    return outimg


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
        print "Reading"

        img2 <- either (\_ -> mempty) id `fmap` Image.readImageFromBMP fileIn
        let img3 = imgtest img2


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

