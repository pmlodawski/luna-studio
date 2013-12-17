{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

import           Control.Applicative

import qualified Canny                         as Canny
import qualified Config                        as Cfg
import qualified Monitoring                    as Monitoring
import qualified ParseArgs                     as ParseArgs
import qualified Wildfire                      as Wildfire
                                                   
import           Prelude                       as P
import qualified Data.Label                    as Label
import           Criterion.Main                ( defaultMainWith, bgroup, bench, whnf )
import qualified System.Exit                   as Exit
import qualified System.Environment            as Env
                                                    
import           Data.Monoid                   (mempty, Monoid)
import qualified Data.Array.Accelerate         as A
import           Data.Array.Accelerate         (Acc, Exp, (:.)(..))
import qualified Data.Array.Accelerate.IO      as A
import qualified Data.Array.Repa.IO.BMP        as R
import qualified Data.Array.Repa.Repr.Unboxed  as R
import qualified Data.Array.Repa               as R
import qualified Data.Map                      as Map
import           Data.Map                      (Map)
import qualified Data.Array.Repa.IO.DevIL       as DevIL

import           System.TimeIt                 (timeIt)

import           Data.Array.Repa.Eval          (Target)
import           Data.Word          (Word8)

import qualified Flowbox.Graphics.Raster.Image    as Image
import qualified Flowbox.Graphics.Raster.Image.IO as Image
import           Flowbox.Graphics.Raster.Image    (Image)
import qualified Flowbox.Graphics.Raster.Channel  as Channel
import           Flowbox.Graphics.Raster.Channel  (Channel)

--import           Control.Monad


import qualified Data.Array.Repa.Eval           as R

import Data.Bits ((.&.))


import qualified Data.Array.Accelerate.CUDA             as CUDA


normalize :: (R.Target a Double, R.Source a Double, R.Source a Word8, Monad m, R.Shape dim) => 
             R.Array a dim Word8 -> m (R.Array a dim Double)
normalize arr = R.computeP $ R.map ffs arr
                where {-# INLINE ffs #-}
                      ffs     :: Word8 -> Double
                      ffs x   =  fromIntegral (fromIntegral x :: Int)
{-# INLINE normalize #-}











--instance Functor Channel where
--    fmap f chan = case chan of
--        AccChannel m -> AccChannel $ A.map f m 


--data Channel a = RepaChannel (R.Array R.U R.DIM2 a)
--               | AccChannel  (A.Array     A.DIM2 a)


--instance Show (Channel a) where
--    show chan = case chan of
--        RepaChannel {} -> "RepaChannel"
--        AccChannel  {} -> "AccChannel"

    --decomposeIL :: (Monad m) => DevIL.Image -> m (Image Double)
    --decomposeIL (DevIL.BGR m) = do
    --    nm <- normalize m
    --    r  <- R.computeP $ R.slice nm (R.Any R.:. R.All R.:. R.All R.:. (2::Int))
    --    g  <- R.computeP $ R.slice nm (R.Any R.:. R.All R.:. R.All R.:. (1::Int))
    --    b  <- R.computeP $ R.slice nm (R.Any R.:. R.All R.:. R.All R.:. (0::Int))
    --    return $ Image 
    --           $ Map.insert "red"   ( RepaChannel r )
    --           $ Map.insert "green" ( RepaChannel g )
    --           $ Map.insert "blue"  ( RepaChannel b )
    --           $ Map.empty
    --{-# INLINE decomposeIL #-}


    --readImage :: FilePath -> IO (Image Double)
    --readImage path = do
    --    imgIl <- DevIL.runIL $ DevIL.readImage path
    --    img   <- decomposeIL imgIl
    --    return img
    --{-# INLINE readImage #-}


demote  :: R.Array A.A R.DIM2 Float -> IO (R.Array R.U R.DIM2 Word8)
demote arr = R.computeP $ R.map ffs arr where 
    {-# INLINE ffs #-}
    ffs     :: Float -> Word8
    ffs x   =  P.fromIntegral (P.truncate x :: Int)
{-# NOINLINE demote #-}


demote2  :: R.Array A.A R.DIM2 Word8 -> IO (R.Array R.U R.DIM2 Word8)
demote2 arr = R.computeP $ R.map id arr where 
{-# NOINLINE demote2 #-}


--toGreyscale :: Acc (Image RGBA) -> Acc (Image Float)
--toGreyscale = A.map (\rgba -> 255 * luminanceOfRGBA32 rgba


--luminanceOfRGBA32 :: (Elt a, IsFloating a) => Exp RGBA32 -> Exp a
--luminanceOfRGBA32 rgba =
--  let r = 0.3  * A.fromIntegral (rgba                 .&. 0xFF)
--      g = 0.59 * A.fromIntegral ((rgba `div` 0x100)   .&. 0xFF)
--      b = 0.11 * A.fromIntegral ((rgba `div` 0x10000) .&. 0xFF)
--  in
--  (r + g + b) / 255





getRfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word32
getRfromRGBA32 rgba = rgba .&. 0xFF

getGfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word32
getGfromRGBA32 rgba = (rgba `div` 0x100) .&. 0xFF

getBfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word32
getBfromRGBA32 rgba = (rgba `div` 0x10000) .&. 0xFF

getAfromRGBA32 :: Exp A.RGBA32 -> Exp A.Word32
getAfromRGBA32 rgba = (rgba `div` 0x1000000) .&. 0xFF

normalizeColor :: Exp A.Word32 -> Exp A.Float
normalizeColor c = A.fromIntegral c / 255

color2Int :: Exp A.Float -> Exp A.Word8
color2Int c = A.truncate $ c * 255

img2Int :: Image A.Float -> Image A.Word8
img2Int img = Image.map color2Int img

normalizeRGBA32 :: A.Array A.DIM2 A.RGBA32 -> Image Float
normalizeRGBA32 (A.use -> img) = Image.insert "red"   ( getChan getRfromRGBA32 )
                               $ Image.insert "green" ( getChan getGfromRGBA32 )
                               $ Image.insert "blue"  ( getChan getBfromRGBA32 )
                               $ Image.insert "alpha" ( getChan getAfromRGBA32 )
                               $ mempty
                               where getChan f = Channel.Raw $ CUDA.run $ A.map (normalizeColor . f) img

luminance :: Image Float -> Maybe (Channel Float)
luminance img = out
    where chr = fmap Channel.use $ Image.lookup "red"   img
          chg = fmap Channel.use $ Image.lookup "green" img
          chb = fmap Channel.use $ Image.lookup "blue"  img
          colormix r g b = 0.3 * r + 0.59 * g + 0.11 * b
          out = Channel.Acc <$> ((A.zipWith3 colormix) <$> chr <*> chg <*> chb)


luminance2 rname gname bname outname img = flip (Image.insert outname) img <$> out
    where chr = fmap Channel.use $ Image.lookup rname img
          chg = fmap Channel.use $ Image.lookup gname img
          chb = fmap Channel.use $ Image.lookup bname img
          colormix r g b = 0.3 * r + 0.59 * g + 0.11 * b
          out = Channel.Acc <$> ((A.zipWith3 colormix) <$> chr <*> chg <*> chb)



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

        -- Read in the image file
        print "Reading"
        imgx <- either (\_ -> mempty) id `fmap` Image.readImageFromBMP fileIn 
        let imgf = Image.reprFloat imgx :: Image Float

        --img   <- timeIt $ (either (error . show) id `fmap` A.readImageFromBMP fileIn :: IO (A.Array A.DIM2 A.Word32))
        --let imgn = normalizeRGBA32 img :: Image Float

        let Just lchan = luminance imgf
            Just imgf2 = fmap Image.reprWord8 
                       $ fmap (Image.cpChannel "luminance" "red")
                       $ fmap (Image.cpChannel "luminance" "green")
                       $ fmap (Image.cpChannel "luminance" "blue")
                       $ luminance2 "red" "green" "blue" "luminance" imgf
            Just outarr = fmap CUDA.run $ Image.encodeRGBA32 imgf2


            Channel.Raw ltest = Channel.compute $ Channel.map color2Int lchan

        A.writeImageToBMP fileOut outarr
        -- Set up the algorithm parameters
            --let threshLow :: Float
            --    threshLow   = Label.get Cfg.configThreshLow conf
            --    threshHigh  = Label.get Cfg.configThreshHigh conf
            --    backend     = Label.get Cfg.configBackend conf

            --    -- Set up the partial results so that we can benchmark individual
            --    -- kernel stages.
            --    low                 = A.constant threshLow
            --    high                = A.constant threshHigh

            --    grey'               = ParseArgs.run backend $ Canny.toGreyscale (A.use img)
            --    blurX'              = ParseArgs.run backend $ Canny.gaussianX (A.use grey')
            --    blurred'            = ParseArgs.run backend $ Canny.gaussianY (A.use blurX')
            --    magdir'             = ParseArgs.run backend $ Canny.gradientMagDir low (A.use blurred')
            --    suppress'           = ParseArgs.run backend $ Canny.nonMaximumSuppression low high (A.use magdir')


        --let test            = A.toRepa grey'
        --test2 <- (demote test :: IO (R.Array R.U R.DIM2 Word8))


        if P.not (Label.get Cfg.configBenchmark conf)
           then do
             -- Connect the strong and weak edges of the image using Repa, and
             -- write the final image to file
             --
             --let (image, strong) = ParseArgs.run backend $ A.lift (Canny.canny threshLow threshHigh (A.use img))
             --edges              <- Wildfire.wildfire (A.toRepa image) (A.toRepa strong)
             --R.writeImageToBMP fileOut (R.zip3 edges edges edges)


                 --print "!!!1"
                 ---- Connect the strong and weak edges of the image using Repa, and
                 ---- write the final image to file
                 ----
                 --print "1"
                 --let (image, strong) = ParseArgs.run backend $ A.lift (Canny.canny threshLow threshHigh (A.use img))
                 --print "2"
             let test            = A.toRepa ltest -- :: Int -- R.Array R.U R.DIM2 Word8
                 ----print (test `R.deepSeq` "#2")
                 --print "3"
             test2 <- timeIt (demote2 test :: IO (R.Array R.U R.DIM2 Word8))
                 --print "4"
                 ----edges              <- Wildfire.wildfire (A.toRepa image) (A.toRepa strong)
                 --print "write"
             --timeIt $ R.writeImageToBMP fileOut (R.zip3 test2 test2 test2)
             return ()

          else do
            -- Run each of the individual kernel stages through criterion, as
            -- well as the end-to-end step process.
            --
            Env.withArgs (P.drop 2 nops) $ defaultMainWith cconf (return ())
              [ bgroup "kernels"
                [   --  bench "normalize"   $ whnf normalizeRGBA32 img
                    --, bench "demote"      $ (demote test :: IO (R.Array R.U R.DIM2 Word8))
                    --, bench "file read"   $ either (error . show) id `fmap` A.readImageFromBMP fileIn
                    --, bench "greyscale"   $ whnf ((ParseArgs.run backend) . Canny.toGreyscale) (A.use img)
                    --, bench "cos"         $ whnf (\img' -> ParseArgs.run backend $ A.lift (Canny.canny threshLow threshHigh (A.use img'))) img
                    --, bench "toRepa"      $ whnf (A.toRepa) grey'
                    --, bench "write"       $ R.writeImageToBMP fileOut (R.zip3 test2 test2 test2)
                ]

              ]

            --Env.withArgs (P.drop 2 nops) $ defaultMainWith cconf (return ())
            --  [ bgroup "kernels"
            --    [ bench "greyscale"   $ whnf (ParseArgs.run1 backend Canny.toGreyscale) img
            --    , bench "blur-x"      $ whnf (ParseArgs.run1 backend Canny.gaussianX) grey'
            --    , bench "blur-y"      $ whnf (ParseArgs.run1 backend Canny.gaussianY) blurX'
            --    , bench "grad-x"      $ whnf (ParseArgs.run1 backend Canny.gradientX) blurred'
            --    , bench "grad-y"      $ whnf (ParseArgs.run1 backend Canny.gradientY) blurred'
            --    , bench "mag-orient"  $ whnf (ParseArgs.run1 backend (Canny.gradientMagDir low)) blurred'
            --    , bench "suppress"    $ whnf (ParseArgs.run1 backend (Canny.nonMaximumSuppression low high)) magdir'
            --    , bench "select"      $ whnf (ParseArgs.run1 backend Canny.selectStrong) suppress'
            --    ]

            --  , bgroup "canny"
            --    [ bench "run"     $ whnf (ParseArgs.run backend . (P.snd . Canny.canny threshLow threshHigh)) (A.use img)
            --    , bench "run1"    $ whnf (ParseArgs.run1 backend  (P.snd . Canny.canny threshLow threshHigh)) img
            --    ]
            --  ]

