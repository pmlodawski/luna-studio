{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP           #-}

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
import qualified Debug.Trace                  as D

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

import qualified Data.Array.Accelerate.Interpreter      as Interp

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



--imgtest :: Image A.Word32 -> Either Image.Error (Image A.Word32)
imgtest img imgFilter = do
    rgba  <- Image.reprFloat <$> RGBA.decompose img
    rgbaFilter <- Image.reprFloat <$> RGBA.decompose imgFilter
    --lrgba <- luminance' rgba
           -- >>= Image.cpChannel "luminance" "r"
           -- >>= Image.cpChannel "luminance" "g"
           -- >>= Image.cpChannel "luminance" "b"
    --lrgba <- adjustCB 2.2 0.2 "r" "g" "b" rgba
    --let blur3x3 = [0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1]
        --blur5x5 = [0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04,0.04]
        --sharpen3x3 = [-1.0,-1.0,-1.0,-1.0,9.0,-1.0,-1.0,-1.0,-1.0]
    --lrgba <- convolve "r" convolve5x5 sharpen3x3 rgba
    --hsv <- convertRGBtoHSV rgba
    --h <- Image.lookup "h" hsv
    --s <- Image.lookup "s" hsv
    --v <- Image.lookup "v" hsv
    --let hsvl = Image.insert "h" (Channel.map (mod1 . (+0.5)) h) $ hsv
    --let hsvl = Image.insert "s" (clipValues $ Channel.map (+1) s) $ hsv
    --lrgba <- convertHSVtoRGB hsv
    lrgba <- blendRGB rgba rgbaFilter (blenderAlpha 0.5) -- ...
    RGBA.compose $ Image.reprWord8 lrgba
    where nonIntRem x y = x - (y * (A.fromIntegral $ (A.truncate (x / y) :: Exp Int)))
          mod1 = flip nonIntRem 1.0

-- convolution

convolve3x3 :: (A.Elt a, A.IsNum a) => [A.Exp a] -> A.Stencil3x3 a -> A.Exp a
convolve3x3 kernel ((a,b,c),(d,e,f),(g,h,i))
    = P.sum $ P.zipWith (*) kernel [a,b,c,d,e,f,g,h,i]

convolve3x5 :: (A.Elt a, A.IsNum a) => [A.Exp a] -> A.Stencil3x5 a -> A.Exp a
convolve3x5 kernel ((a,b,c),(d,e,f),(g,h,i),(j,k,l),(m,n,o))
    = P.sum $ P.zipWith (*) kernel [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]

convolve5x3 :: (A.Elt a, A.IsNum a) => [A.Exp a] -> A.Stencil5x3 a -> A.Exp a
convolve5x3 kernel ((a,b,c,d,e),(f,g,h,i,j),(k,l,m,n,o))
    = P.sum $ P.zipWith (*) kernel [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]

convolve5x5 :: (A.Elt a, A.IsNum a) => [A.Exp a] -> A.Stencil5x5 a -> A.Exp a
convolve5x5 kernel ((a,b,c,d,e),(f,g,h,i,j),(k,l,m,n,o),(p,q,r,s,t),(u,v,w,x,y))
    = P.sum $ P.zipWith (*) kernel [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y]

--convolve :: Int
--convolve :: String -> (t0 -> stencil0 -> Exp Float) -> t0 -> Image Float -> Either Image.Error (Image Float)
convolve cname convolution kernel img = do
    channel <- Image.lookup cname img
    let outimg = Image.insert cname channel'
               $ img
        channel' = Channel.Acc $ A.stencil (convolution kernel) A.Clamp (Channel.accMatrix channel)
    return outimg

--convolveRGB :: Int
--convolveRGB :: ([Exp a2] -> A.Stencil5x5 a2 -> Exp a2) -> [Float] -> Image Float -> Either Image.Error (Image Float)
--convolveRGB :: ([Exp a2] -> A.Stencil5x5 a2 -> Exp a2) -> [Exp Float] -> Image Float -> Either Image.Error (Image Float)
--convolveRGB convolution kernel img = do
--    let outimg = convolve "r" convolution kernel
--               $ convolve "g" convolution kernel
--               $ convolve "b" convolution kernel
--               $ img
--    return outimg

-- brightness and contrast

adjustCB :: A.Exp Float -> A.Exp Float -> String -> String -> String -> (Image Float) -> Either Image.Error (Image Float)
adjustCB contrast brightness rname gname bname img = do
    rchannel <- Image.lookup rname img
    gchannel <- Image.lookup gname img
    bchannel <- Image.lookup bname img
    let outimg = Image.insert rname rchannel'
               $ Image.insert gname gchannel'
               $ Image.insert bname bchannel'
               $ img
        rchannel' = clipValues $ Channel.Acc $ A.map adjust (Channel.accMatrix rchannel)
        gchannel' = clipValues $ Channel.Acc $ A.map adjust (Channel.accMatrix gchannel)
        bchannel' = clipValues $ Channel.Acc $ A.map adjust (Channel.accMatrix bchannel)
        adjust x = contrast * x + brightness
    return outimg

contrast :: A.Exp Float -> String -> String -> String -> (Image Float) -> Either Image.Error (Image Float)
contrast x r g b = adjustCB x 0 r g b

brightness :: A.Exp Float -> String -> String -> String -> (Image Float) -> Either Image.Error (Image Float)
brightness x r g b = adjustCB 1 x r g b



-- color conversion

clipValues :: (Channel Float) -> (Channel Float)
clipValues channel = Channel.Acc $ A.map (P.max 0 . P.min 1) $ Channel.accMatrix channel

--calculateHueFromRGB :: Exp Float -> Exp Float -> Exp Float -> Exp Float
-- calculateHueFromRGB r g b = ((60 * (h - d / (maxRGB - minRGB))) `F.mod'` 360) / 360
calculateHueFromRGB :: Exp Float -> Exp Float -> Exp Float -> Exp Float
calculateHueFromRGB r g b = w / 6
    --where h      = if r A.==* minRGB then 3 else (if b A.==* minRGB then 1 else 5)
          --d      = if r A.==* minRGB then g-b else (if b A.==* minRGB then r-g else b-r)
    --where h      = r A.==* minRGB A.? (3, (b A.==* minRGB A.? (1,5)))
          --d      = r A.==* minRGB A.? (g-b, (b A.==* minRGB A.? (r-g,b-r)))
           -- mod 6 ?!?!?! tam niżej
    where w      = r A.==* maxRGB A.? (((g - b) / delta) `nonIntRem` 6,
                   g A.==* maxRGB A.? ((b - r) / delta + 2,
                   (r - g) / delta + 4
                   ))
          minRGB = P.min r $ P.min g b
          maxRGB = P.max r $ P.max g b
          delta  = maxRGB - minRGB
          nonIntRem x y = x - (y * (A.fromIntegral $ (A.truncate (x / y) :: Exp Int)))

calculateSaturationFromRGB :: Exp Float -> Exp Float -> Exp Float -> Exp Float
calculateSaturationFromRGB r g b = (maxRGB - minRGB) / maxRGB
    where maxRGB = P.max r $ P.max g b
          minRGB = P.min r $ P.min g b

calculateValueFromRGB :: Exp Float -> Exp Float -> Exp Float -> Exp Float
calculateValueFromRGB r g b = P.max r $ P.max g b

convertRGBtoHSV :: (Image Float) -> Either Image.Error (Image Float)
convertRGBtoHSV img = do
    r <- Image.lookup "r" img
    g <- Image.lookup "g" img
    b <- Image.lookup "b" img
    let outimg     = Image.insert "h" hue
                   $ Image.insert "s" saturation
                   $ Image.insert "v" value
                   $ img
        hue        = Channel.zipWith3 calculateHueFromRGB r g b
        saturation = Channel.zipWith3 calculateSaturationFromRGB r g b
        value      = Channel.zipWith3 calculateValueFromRGB r g b
    return outimg

calculateRGBfromHSV :: Exp Float -> Exp Float -> Exp Float -> (Exp Float, Exp Float, Exp Float)
calculateRGBfromHSV h s v = A.unlift a :: (Exp Float, Exp Float, Exp Float)
    where a = i A.==* (0::Exp Int) A.? (A.lift ((v, t, p) :: (Exp Float, Exp Float, Exp Float)),
              i A.==* (1::Exp Int) A.? (A.lift ((q, v, p) :: (Exp Float, Exp Float, Exp Float)),
              i A.==* (2::Exp Int) A.? (A.lift ((p, v, t) :: (Exp Float, Exp Float, Exp Float)),
              i A.==* (3::Exp Int) A.? (A.lift ((p, q, v) :: (Exp Float, Exp Float, Exp Float)),
              i A.==* (4::Exp Int) A.? (A.lift ((t, p, v) :: (Exp Float, Exp Float, Exp Float)),
              A.lift ((v, p, q) :: (Exp Float, Exp Float, Exp Float))
              )))))
          hi = h * 6
          i = A.floor hi :: Exp (A.Plain Int)
          f = hi - (A.fromIntegral i)
          p = v * (1 - s)
          q = v * (1 - s * f)
          t = v * (1 - s * (1 - f))

convertHSVtoRGB :: (Image Float) -> Either Image.Error (Image Float)
convertHSVtoRGB img = do
    h <- Image.lookup "h" img
    s <- Image.lookup "s" img
    v <- Image.lookup "v" img
    let outimg = Image.insert "r" red
               $ Image.insert "g" green
               $ Image.insert "b" blue
               $ img
        red    = Channel.zipWith3 calcR h s v
        green  = Channel.zipWith3 calcG h s v
        blue   = Channel.zipWith3 calcB h s v
        calcR  = (\a b c -> let (z, _, _) = calculateRGBfromHSV a b c in z)
        calcG  = (\a b c -> let (_, z, _) = calculateRGBfromHSV a b c in z)
        calcB  = (\a b c -> let (_, _, z) = calculateRGBfromHSV a b c in z)
    return outimg



-- blending
blendC :: (Channel Float) -> (Channel Float) -> ((Exp Float) -> (Exp Float) -> (Exp Float)) -> (Channel Float)
blendC channelA channelB blender = Channel.zipWith blender channelA channelB



blendRGB :: (Image Float) -> (Image Float) -> ((Exp Float) -> (Exp Float) -> (Exp Float)) -> Either Image.Error (Image Float)
blendRGB img1 img2 blender = do
    r1 <- Image.lookup "r" img1
    g1 <- Image.lookup "g" img1
    b1 <- Image.lookup "b" img1
    a1 <- Image.lookup "a" img1
    r2 <- Image.lookup "r" img2
    g2 <- Image.lookup "g" img2
    b2 <- Image.lookup "b" img2
    a2 <- Image.lookup "a" img2
    let outimg = Image.insert "r" r'
               $ Image.insert "g" g'
               $ Image.insert "b" b'
               $ Image.insert "a" a'
               $ mempty
        r'     = blendC r1 r2 blender
        g'     = blendC g1 g2 blender
        b'     = blendC b1 b2 blender
        a'     = blendC a1 a2 blender
    return outimg

-- #define ChannelBlend_Normal(A,B)     ((uint8)(A))
blenderNormal :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderNormal a b = a

-- #define ChannelBlend_Lighten(A,B)    ((uint8)((B > A) ? B:A))
blenderLighten :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderLighten a b = b A.>* a A.? (b,a)

-- #define ChannelBlend_Darken(A,B)     ((uint8)((B > A) ? A:B))
blenderDarken :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderDarken a b = b A.>* a A.? (a,b)

-- #define ChannelBlend_Multiply(A,B)   ((uint8)((A * B) / 255))
blenderMultiply :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderMultiply a b = (a * b)

-- #define ChannelBlend_Average(A,B)    ((uint8)((A + B) / 2))
blenderAverage :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderAverage a b = (a + b) / 2

-- #define ChannelBlend_Add(A,B)        ((uint8)(min(255, (A + B))))
blenderAdd :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderAdd a b = 1 A.<* (a + b) A.? (1 , a + b)

-- #define ChannelBlend_Subtract(A,B)   ((uint8)((A + B < 255) ? 0:(A + B - 255)))
blenderSubtract :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderSubtract a b = (a + b) A.<* 1 A.? (0 , a + b - 1)

-- #define ChannelBlend_Difference(A,B) ((uint8)(abs(A - B)))
blenderDifference :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderDifference a b = a A.>* b A.? (a - b , b - a)

-- #define ChannelBlend_Negation(A,B)   ((uint8)(255 - abs(255 - A - B)))
blenderNegation :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderNegation a b = 1 - (1 - a - b A.>* 0 A.? (1 - a - b , -(1 - a - b)))

-- #define ChannelBlend_Screen(A,B)     ((uint8)(255 - (((255 - A) * (255 - B)) >> 8)))
blenderScreen :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderScreen a b = 1 - (1 - a) * (1 - b)

-- #define ChannelBlend_Exclusion(A,B)  ((uint8)(A + B - 2 * A * B / 255))
blenderExclusion :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderExclusion a b = a + b - 2 * a * b

-- #define ChannelBlend_Overlay(A,B)    ((uint8)((B < 128) ? (2 * A * B / 255):(255 - 2 * (255 - A) * (255 - B) / 255)))
blenderOverlay :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderOverlay a b = b A.<* 0.5 A.? ((2 * a * b) , (1 - 2 * (1 - a) * (1 - b)))

-- #define ChannelBlend_SoftLight(A,B)  ((uint8)((B < 128)?(2*((A>>1)+64))*((float)B/255):(255-(2*(255-((A>>1)+64))*(float)(255-B)/255))))
blenderSoftLight :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderSoftLight a b = b A.<* 0.5 A.? (2 * (a / 2 + 0.25) * b , 1 - 2 * (1 - (a / 2 + 0.25)) * (1-b))

-- #define ChannelBlend_HardLight(A,B)  (ChannelBlend_Overlay(B,A))
blenderHardLight :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderHardLight = flip blenderOverlay

-- #define ChannelBlend_ColorDodge(A,B) ((uint8)((B == 255) ? B:min(255, ((A << 8 ) / (255 - B)))))
blenderColorDodge :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderColorDodge a b = b A.>=* 1 A.? (b , 1 A.<* a / (1 - b) A.? (1 , a / (1 - b)))

-- #define ChannelBlend_ColorBurn(A,B)  ((uint8)((B == 0) ? B:max(0, (255 - ((255 - A) << 8 ) / B))))
blenderColorBurn :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderColorBurn a b = b A.<=* 0 A.? (b , 0 A.>* 1 - (1 - a) / b A.? (0 , 1 - (1 - a) / b))

-- #define ChannelBlend_LinearDodge(A,B)(ChannelBlend_Add(A,B))
blenderLinearDodge :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderLinearDodge = blenderAdd

-- #define ChannelBlend_LinearBurn(A,B) (ChannelBlend_Subtract(A,B))
blenderLinearBurn :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderLinearBurn = blenderSubtract

-- #define ChannelBlend_LinearLight(A,B)((uint8)(B < 128)?ChannelBlend_LinearBurn(A,(2 * B)):ChannelBlend_LinearDodge(A,(2 * (B - 128))))
blenderLinearLight :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderLinearLight a b = b A.<* 0.5 A.? (blenderLinearBurn a (2 * b) , blenderLinearDodge a (2 * (b - 0.5)))

-- #define ChannelBlend_VividLight(A,B) ((uint8)(B < 128)?ChannelBlend_ColorBurn(A,(2 * B)):ChannelBlend_ColorDodge(A,(2 * (B - 128))))
blenderVividLight :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderVividLight a b = b A.<* 0.5 A.? (blenderColorBurn a (2 * b) , blenderColorDodge a (2 * (b - 0.5)))

-- #define ChannelBlend_PinLight(A,B)   ((uint8)(B < 128)?ChannelBlend_Darken(A,(2 * B)):ChannelBlend_Lighten(A,(2 * (B - 128))))
blenderPinLight :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderPinLight a b = b A.<* 0.5 A.? (blenderDarken a (2 * b) , blenderLighten a (2 * (b - 0.5)))

-- #define ChannelBlend_HardMix(A,B)    ((uint8)((ChannelBlend_VividLight(A,B) < 128) ? 0:255))
blenderHardMix :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderHardMix a b = blenderVividLight a b A.<* 0.5 A.? (0 , 1)

-- #define ChannelBlend_Reflect(A,B)    ((uint8)((B == 255) ? B:min(255, (A * A / (255 - B)))))
blenderReflect :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderReflect a b = b A.>=* 1 A.? (b , 1 A.<* a * a / (1 - b) A.? (1 , a * a / (1 - b)))

-- #define ChannelBlend_Glow(A,B)       (ChannelBlend_Reflect(B,A))
blenderGlow :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderGlow = flip blenderReflect

-- #define ChannelBlend_Phoenix(A,B)    ((uint8)(min(A,B) - max(A,B) + 255))
blenderPhoenix :: (Exp Float) -> (Exp Float) -> (Exp Float)
blenderPhoenix a b = (a A.<* b A.? (a , b)) - (a A.>* b A.? (a , b)) + 1

-- #define ChannelBlend_Alpha(A,B,O)    ((uint8)(O * A + (1 - O) * B))
blenderAlpha o a b = (o * a + (1 - o) * b)

-- #define ChannelBlend_AlphaF(A,B,F,O) (ChannelBlend_Alpha(F(A,B),A,O))
blenderAlphaF f o a b = blenderAlpha (f a b) a o



-- main

main :: IO ()
main
  = do
        print "--- graphic tests ---"
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
        imgFilter <- either (\_ -> mempty) id `fmap` Image.readImageFromBMP "filter.bmp"
        let img3 = imgtest img2 imgFilter

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

