 -- [ ] 1) lut
 -- [ ] 2) dylatacja
 -- [ ] 3) erozja
 -- [ ] 4) otwarcie
 -- [ ] 5) zamknięcie
 -- [ ] 6) mediana - do czysczenia np. kurzu
 -- [ ] 7) zmiany kolorow na krzywych - definiowane za pomoca datatypu, ktory okrelalby czy to jest linear, bezier cyz cos innego
 -- [ ] 8) samplowanie po kolorach


{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP           #-}

module Flowbox.Graphics.Algorithms where

import Control.Applicative

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


-- basic

invert :: Num a => a -> a
invert x = 1 - x

invert' :: Num a => a -> a
invert' x = -x

sign :: Num a => a -> a
sign x = (2 * x) - 1

parametrize :: Num a => a -> a -> a -> a
parametrize lo hi x = lo + x * (hi - lo)

--bias :: Exp a -> Exp b -> Exp b
bias b x = (b A.>* 0) A.? (x ^ (log(b) / log(0.5)) , 0)

--gain :: Exp a -> Exp b -> Exp a
gain g x = 0.5 * (x A.<* 0.5 A.? (bias (2 * x) (1 - g) , 2 - bias (2 - 2 * x) (1 - g)))

--gamma ::
gamma g x = x ^ (1 / g)

compress :: Num a => a -> a -> a -> a
compress lo hi x = (hi - lo) * x + lo

--expand :: Num a => a -> a -> a -> a
expand lo hi x = lo A.==* hi A.? (x A.<* lo A.? (0 , 1) , (x - lo) / (hi - lo)) -- WATCH OUT! comparing Floating numbers!

--remap :: Num a => a -> a -> a -> a -> a -> a
remap loA hiA loB hiB x = (x * (hiB-loB) - loA*hiB + hiA*loB) / (hiA-loA)



-- simple

binarizeChannel :: (Exp Float -> Exp Bool) -> Channel Float -> Channel Float
binarizeChannel f channel = Channel.map (\x -> f x A.? (1 , 0)) channel

binarizeImage :: (String, String, String) -> (Exp Float -> Exp Bool) -> Image Float -> Either Image.Error (Image Float)
binarizeImage names f img = do
  let (nameA,_,_) = names
      (_,nameB,_) = names
      (_,_,nameC) = names
  channelA <- Image.lookup nameA img
  channelB <- Image.lookup nameB img
  channelC <- Image.lookup nameC img
  let outimg = Image.insert nameA channelA'
             $ Image.insert nameB channelB'
             $ Image.insert nameC channelC'
             $ img
      channelA' = binarizeChannel f channelA
      channelB' = binarizeChannel f channelB
      channelC' = binarizeChannel f channelC
  return outimg

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

--erosion :: Channel Float -> Channel Float
--erosion channel = do...


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


--convolve cname convolution kernel img = do
--convolve channel convolution kernel img = --do
    --channel <- Image.lookup cname img
    --let outimg = Image.insert cname channel'
               -- $ img
    --let channel' =
      --Channel.Acc $ A.stencil (convolution kernel) A.Clamp (Channel.accMatrix channel)
    --return channel'

convolveRGB convolution kernel img = do
    rchannel <- Image.lookup "r" img
    gchannel <- Image.lookup "g" img
    bchannel <- Image.lookup "b" img
    let outimg = Image.insert "r" r'
               $ Image.insert "g" g'
               $ Image.insert "b" b'
               $ img
        r' = clipValues $ Channel.Acc $ A.stencil (convolution kernel) A.Clamp (Channel.accMatrix rchannel)
        g' = clipValues $ Channel.Acc $ A.stencil (convolution kernel) A.Clamp (Channel.accMatrix gchannel)
        b' = clipValues $ Channel.Acc $ A.stencil (convolution kernel) A.Clamp (Channel.accMatrix bchannel)
    return outimg

-- brightness and contrast

adjustCB_RGB :: A.Exp Float -> A.Exp Float -> (Image Float) -> Either Image.Error (Image Float)
adjustCB_RGB = adjustCB "r" "g" "b"

adjustCB :: String -> String -> String -> A.Exp Float -> A.Exp Float -> (Image Float) -> Either Image.Error (Image Float)
adjustCB rname gname bname contrast brightness img = do
    rchannel <- Image.lookup rname img
    gchannel <- Image.lookup gname img
    bchannel <- Image.lookup bname img
    let outimg = Image.insert rname rchannel'
               $ Image.insert gname gchannel'
               $ Image.insert bname bchannel'
               $ img
        rchannel' = clipValues $ Channel.map adjust rchannel
        gchannel' = clipValues $ Channel.map adjust gchannel
        bchannel' = clipValues $ Channel.map adjust bchannel
        adjust x = contrast * x + brightness
    return outimg

contrast :: A.Exp Float -> String -> String -> String -> (Image Float) -> Either Image.Error (Image Float)
contrast x r g b = adjustCB r g b x 0

brightness :: A.Exp Float -> String -> String -> String -> (Image Float) -> Either Image.Error (Image Float)
brightness x r g b = adjustCB r g b 1 x


-- color conversion

clipValues :: (Channel Float) -> (Channel Float)
clipValues channel = Channel.map (P.max 0 . P.min 1) channel


calculateHueFromRGB :: Exp Float -> Exp Float -> Exp Float -> Exp Float
calculateHueFromRGB r g b = (w A.>* 0 A.? (w , w + 6)) / 6
    where w      = delta A.==* 0 A.? (0,
                   r A.==* maxRGB A.? (((g - b) / delta) `nonIntRem` 6,
                   g A.==* maxRGB A.? ((b - r) / delta + 2,
                   (r - g) / delta + 4
                   )))
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
              i A.==* (5::Exp Int) A.? (A.lift ((v, p, q) :: (Exp Float, Exp Float, Exp Float)),
              A.lift ((v, t, p) :: (Exp Float, Exp Float, Exp Float))
              ))))))
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



--- keying

keyColor :: (String, String, String) -> (Exp Float, Exp Float, Exp Float) -> (Exp Float, Exp Float, Exp Float)
            -> (Exp Float -> Exp Float) -> Image Float -> Either Image.Error (Image Float)
keyColor names epsilon value f img = do
  let (nameA,_,_) = names
      (_,nameB,_) = names
      (_,_,nameC) = names
  channelA <- Image.lookup nameA img
  channelB <- Image.lookup nameB img
  channelC <- Image.lookup nameC img
  let outimg = Image.insert nameA channelA'
             $ Image.insert nameB channelB'
             $ Image.insert nameC channelC'
             $ img
      channelA' = match channelA
      channelB' = match channelB
      channelC' = match channelC
      match c = Channel.zipWith4 (\p q r s -> (matched p q r) A.? (f s , s)) channelA channelB channelC c
      matched a b c = (test a valueA epsilonA) A.&&* (test b valueB epsilonB) A.&&* (test c valueC epsilonC)
      test x y z = (delta A.>* 0 A.&&* delta A.<* z) A.||* (delta A.<* 0 A.&&* delta A.>* -z)
                 A.? (A.constant True , A.constant False)
               where delta = x - y
      (epsilonA,_,_) = epsilon
      (_,epsilonB,_) = epsilon
      (_,_,epsilonC) = epsilon
      (valueA,_,_) = value
      (_,valueB,_) = value
      (_,_,valueC) = value
  return outimg
