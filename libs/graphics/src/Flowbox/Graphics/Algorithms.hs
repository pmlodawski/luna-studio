---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

 -- [+] 1) lut
 -- [+] 2) dylatacja
 -- [+] 3) erozja
 -- [+] 4) otwarcie
 -- [+] 5) zamknięcie
 -- [+] 6) mediana - do czysczenia np. kurzu
 -- [ ] 7) zmiany kolorow na krzywych - definiowane za pomoca datatypu, ktory okrelalby czy to jest linear, bezier cyz cos innego
 -- [ ] 8) samplowanie po kolorach

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Graphics.Algorithms where

import           Data.Array.Accelerate             (Exp)
import qualified Data.Array.Accelerate             as A

import           Flowbox.Graphics.Raster.Channel   (Channel)
import qualified Flowbox.Graphics.Raster.Channel   as Channel
import           Flowbox.Graphics.Raster.Image     (Image)
import qualified Flowbox.Graphics.Raster.Image     as Image
import           Flowbox.Prelude                   as P


type String3 = (String, String, String)
type Exp3 a  = (Exp a, Exp a, Exp a)

-- utils

epsilon :: (A.Elt a, A.IsNum a) => Exp a -> Exp a -> Exp a -> Exp Bool
epsilon a b eps = ((delta A.>=* 0) A.&&* (delta A.<* eps)) A.||* ((delta A.<* 0) A.&&* (delta A.>* -eps))
           A.? (A.constant True , A.constant False)
         where delta = a - b

epsilon' :: (A.Elt a, A.IsNum a) => Exp a -> Exp a -> Exp Bool
epsilon' delta eps = ((delta A.>=* 0) A.&&* (delta A.<* eps)) A.||* ((delta A.<* 0) A.&&* (delta A.>* -eps))
           A.? (A.constant True , A.constant False)

applyToImage :: (Channel a -> Channel a) -> String3 -> Image a -> Either Image.Error (Image a)
applyToImage f (nameA, nameB, nameC) img = do
  channelA <- Image.lookup nameA img
  channelB <- Image.lookup nameB img
  channelC <- Image.lookup nameC img
  let outimg = Image.insert nameA (f channelA)
             $ Image.insert nameB (f channelB)
             $ Image.insert nameC (f channelC)
             $ img
  return outimg


-- works assuming we have a sorted array
median :: Fractional a => [a] -> a
median xs | odd  len = xs !! mid
           | even len = meanMedian
                 where  len = length xs
                        mid = len `div` 2
                        meanMedian = (xs !! mid + xs !! (mid+1)) / 2

median' :: (A.Elt a, A.IsFloating a) => A.Acc (A.Vector a) -> Exp a
median' arr = A.cond (A.odd len)
    (arr A.!! mid)
    (meanMedian)
    where len = A.size arr
          mid = A.floor $ (A.fromIntegral len :: Exp Double) / 2
          meanMedian = ((arr A.!! mid) + (arr A.!! (mid+1))) / 2



bsort :: (A.Elt a, A.IsScalar a) => [Exp a] -> [Exp a]
bsort arr = bsortStep arr (length arr)

bsortStep :: (A.Elt a, A.IsScalar a) => [Exp a] -> Int -> [Exp a]
bsortStep arr 0 = arr
bsortStep arr n = bsortStep (bsortPair arr) (n-1)

bsortPair :: (A.Elt a, A.IsScalar a) => [Exp a] -> [Exp a]
bsortPair ([])     = []
bsortPair (a:[])   = [a]
--bsortPair (a:b:[]) = [min a b, max a b]
bsortPair (a:b:t)  = [min a b] ++ (bsortPair $ [max a b] ++ t)

bsort' :: (A.Elt a, A.IsScalar a) => A.Acc (A.Vector a) -> A.Acc (A.Vector a)
bsort' arr = bsortStep' arr $ A.size arr

bsortStep' :: (A.Elt a, A.IsScalar a) => A.Acc (A.Vector a) -> Exp Int -> A.Acc (A.Vector a)
bsortStep' arr n = A.acond (n A.==* 0)
    arr $
    bsortStep' (bsortPair' arr) (n-1)

bsortPair' :: (A.Elt a, A.IsScalar a) => A.Acc (A.Vector a) -> A.Acc (A.Vector a)
bsortPair' arr =
    A.acond (A.null arr)
        (A.take 0 arr) $
        A.acond ((A.size arr) A.==* 1)
          (A.take 1 arr) $
          ((A.flatten . A.minimum) head) A.++ (bsortPair' $ ((A.flatten . A.maximum) head) A.++ tail)
          where head = A.take 2 arr
                tail = A.drop 2 arr



-- basic

invert :: Num a => a -> a
invert x = 1 - x

invert' :: Num a => a -> a
invert' x = -x

sign :: Num a => a -> a
sign x = (2 * x) - 1

parametrize :: Num a => a -> a -> a -> a
parametrize lo hi x = lo + x * (hi - lo)

bias :: (A.Elt t, A.Elt t1, A.IsNum t, A.IsIntegral t1, A.IsFloating t1) => Exp t1 -> Exp t -> Exp t
bias b x = (b A.>* 0) A.? (x ^ (log(b) / log(0.5)) , 0)

gain :: (A.Elt t, A.Elt t1, A.IsIntegral t1, A.IsFloating t, A.IsFloating t1) => Exp t -> Exp t1 -> Exp t
gain g x = 0.5 * (x A.<* 0.5 A.? (bias (2 * x) (1 - g) , 2 - bias (2 - 2 * x) (1 - g)))

gamma :: (Fractional b, Integral b, Num a) => b -> a -> a
gamma g x = x ^ (1 / g)

compress :: Num a => a -> a -> a -> a
compress lo hi x = (hi - lo) * x + lo

expand :: (A.Elt t, A.IsFloating t) => Exp t -> Exp t -> Exp t -> Exp t
expand lo hi x = lo A.==* hi A.? (x A.<* lo A.? (0 , 1) , (x - lo) / (hi - lo)) -- WATCH OUT! comparing Floating numbers!

remap :: Fractional a => a -> a -> a -> a -> a -> a
remap loA hiA loB hiB x = (x * (hiB-loB) - loA*hiB + hiA*loB) / (hiA-loA)



-- simple

lutExp :: (A.Elt a, A.IsFloating a) => [(Exp a, Exp a)] -> Exp a -> Exp a
lutExp [] x = x
lutExp ((_,x):[]) _ = x
lutExp ((a,b):c@((p,q):_)) x = (x A.<* a) A.? ( b
                                           , (a A.<=* x A.&&* x A.<* p) A.?
                                             ( b + (x-a) / (p-a) * ((q-p))
                                             , lutExp c x ))

lutChannel :: (A.Elt a, A.IsFloating a) => [(Exp a, Exp a)] -> Channel a -> Channel a
lutChannel table channel = Channel.map (lutExp table) channel

lutImage :: (A.Elt a, A.IsFloating a) => String3 -> [(Exp a, Exp a)] -> Image a -> Either Image.Error (Image a)
lutImage names table = applyToImage (lutChannel table) names


binarizeChannel :: (A.Elt a, A.IsNum a) => (Exp a -> Exp Bool) -> Channel a -> Channel a
binarizeChannel f channel = Channel.map (\x -> f x A.? (1 , 0)) channel

binarizeImage :: (A.Elt a, A.IsNum a) => (Exp a -> Exp Bool) -> String3 -> Image a -> Either Image.Error (Image a)
binarizeImage f = applyToImage (binarizeChannel f)

luminance :: (A.Elt a, A.IsFloating a) => String3 -> String -> (Image a) -> Either Image.Error (Image a)
luminance (rname, gname, bname) outname img = do
    chr <- Image.lookup rname img
    chg <- Image.lookup gname img
    chb <- Image.lookup bname img
    let chan = Channel.zipWith3 colormix chr chg chb
        colormix r g b = 0.3 * r + 0.59 * g + 0.11 * b
        outimg = Image.insert outname chan img
    return outimg

luminance' :: (A.Elt a, A.IsFloating a) => (Image a) -> Either Image.Error (Image a)
luminance' = luminance ("r", "g", "b") "luminance"



erodeChannel :: (A.Elt a, A.IsFloating a) => Channel a -> Channel a
erodeChannel channel = Channel.stencil erode A.Mirror channel
    where erode ((a,b,c),(d,e,f),(g,h,i)) = minimum [a,b,c,d,e,f,g,h,i]

erodeImage :: (A.Elt a, A.IsFloating a) => String3 -> Image a -> Either Image.Error (Image a)
erodeImage = applyToImage erodeChannel

dilateChannel :: (A.Elt a, A.IsFloating a) => Channel a -> Channel a
dilateChannel channel = Channel.stencil dilate A.Mirror channel
    where dilate ((a,b,c),(d,e,f),(g,h,i)) = maximum [a,b,c,d,e,f,g,h,i]

dilateImage :: (A.Elt a, A.IsFloating a) => String3 -> Image a -> Either Image.Error (Image a)
dilateImage = applyToImage dilateChannel



openImage :: (A.Elt a, A.IsFloating a) => String3 -> Image a -> Either Image.Error (Image a)
openImage names img = do
    imgA <- erodeImage names img
    imgB <- dilateImage names imgA
    return imgB

closeImage :: (A.Elt a, A.IsFloating a) => String3 -> Image a -> Either Image.Error (Image a)
closeImage names img = do
    imgA <- dilateImage names img
    imgB <- erodeImage names imgA
    return imgB



medianChannel :: (A.Elt a, A.IsFloating a) => Channel a -> Channel a
medianChannel channel = Channel.stencil middleValue A.Mirror channel
    where middleValue ((a,b,c),(d,e,f),(g,h,i)) = (bsort [a,b,c,d,e,f,g,h,i]) !! 4 -- 4 is the middle index of 9 which is the length of the window

medianImage :: (A.Elt a, A.IsFloating a) => String3 -> Image a -> Either Image.Error (Image a)
medianImage = applyToImage medianChannel



premultiply :: (A.Elt a, A.IsFloating a) => Image a -> Either Image.Error (Image a)
premultiply img = do
    channelR <- Image.lookup "r" img
    channelG <- Image.lookup "g" img
    channelB <- Image.lookup "b" img
    channelA <- Image.lookup "a" img
    let outimg = Image.insert "r" (premultiplyChannel channelR channelA)
               $ Image.insert "g" (premultiplyChannel channelG channelA)
               $ Image.insert "b" (premultiplyChannel channelB channelA)
               $ img
        premultiplyChannel = Channel.zipWith (\x a -> x * a)
    return outimg


unpremultiply :: (A.Elt a, A.IsFloating a) => Image a -> Either Image.Error (Image a)
unpremultiply img = do
    channelR <- Image.lookup "r" img
    channelG <- Image.lookup "g" img
    channelB <- Image.lookup "b" img
    channelA <- Image.lookup "a" img
    let outimg = Image.insert "r" (unpremultiplyChannel channelR channelA)
               $ Image.insert "r" (unpremultiplyChannel channelG channelA)
               $ Image.insert "r" (unpremultiplyChannel channelB channelA)
               $ img
        unpremultiplyChannel = Channel.zipWith (\x a -> x / a)
    return outimg



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

convolve :: (A.Elt a, A.IsFloating a, A.Stencil A.DIM2 a stencil) =>
  String3 -> (t -> stencil -> Exp a) -> t -> Image a -> Either Image.Error (Image a)
convolve (nameA, nameB, nameC) convolution kernel img = do
  channelA <- Image.lookup nameA img
  channelB <- Image.lookup nameB img
  channelC <- Image.lookup nameC img
  let outimg = Image.insert nameA channelA'
             $ Image.insert nameB channelB'
             $ Image.insert nameC channelC'
             $ img
      channelA' = clipValues $ Channel.stencil (convolution kernel) A.Clamp channelA
      channelB' = clipValues $ Channel.stencil (convolution kernel) A.Clamp channelB
      channelC' = clipValues $ Channel.stencil (convolution kernel) A.Clamp channelC
  return outimg

convolveRGB :: (A.Elt a, A.IsFloating a, A.Stencil A.DIM2 a stencil) =>
  (t -> stencil -> Exp a) -> t -> Image a -> Either Image.Error (Image a)
convolveRGB = convolve ("r", "g", "b")



-- brightness and contrast

adjustCB_RGB :: (A.Elt a, A.IsFloating a) => A.Exp a -> A.Exp a -> Image a -> Either Image.Error (Image a)
adjustCB_RGB = adjustCB ("r", "g", "b")

adjustCB :: (A.Elt a, A.IsFloating a) => String3 -> A.Exp a -> A.Exp a -> Image a -> Either Image.Error (Image a)
adjustCB (rname, gname, bname) contrastValue brightnessValue img = do
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
        adjust x = contrastValue * x + brightnessValue
    return outimg

contrast :: (A.Elt a, A.IsFloating a) => A.Exp a -> String3 -> Image a -> Either Image.Error (Image a)
contrast x rgb = adjustCB rgb x 0

brightness :: (A.Elt a, A.IsFloating a) => A.Exp a -> String3 -> Image a -> Either Image.Error (Image a)
brightness x rgb = adjustCB rgb 1 x


-- color conversion

clipValues :: (A.Elt a, A.IsFloating a) => Channel a -> Channel a
clipValues channel = Channel.map (P.max 0 . P.min 1) channel



calculateHueFromRGB :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a
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

calculateSaturationFromRGB :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a
calculateSaturationFromRGB r g b = (maxRGB - minRGB) / maxRGB
    where maxRGB = P.max r $ P.max g b
          minRGB = P.min r $ P.min g b

calculateValueFromRGB :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a -> Exp a
calculateValueFromRGB r g b = P.max r $ P.max g b

convertRGBtoHSV :: (A.Elt a, A.IsFloating a) => Image a -> Either Image.Error (Image a)
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



calculateRGBfromHSV :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a -> Exp3 a
calculateRGBfromHSV h s v = A.unlift res
    where res = i A.==* (0::Exp Int) A.? (A.lift ((v, t, p)),
                i A.==* (1::Exp Int) A.? (A.lift ((q, v, p)),
                i A.==* (2::Exp Int) A.? (A.lift ((p, v, t)),
                i A.==* (3::Exp Int) A.? (A.lift ((p, q, v)),
                i A.==* (4::Exp Int) A.? (A.lift ((t, p, v)),
                i A.==* (5::Exp Int) A.? (A.lift ((v, p, q)),
                A.lift ((v, t, p))
                ))))))
          hi = h * 6
          i = A.floor hi :: Exp (A.Plain Int)
          f = hi - (A.fromIntegral i)
          p = v * (1 - s)
          q = v * (1 - s * f)
          t = v * (1 - s * (1 - f))

convertHSVtoRGB :: (A.Elt a, A.IsFloating a) => Image a -> Either Image.Error (Image a)
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

blendC :: (A.Elt a, A.IsFloating a) => Channel a -> Channel a -> (Exp a -> Exp a -> Exp a) -> Channel a
blendC channelA channelB blender = Channel.zipWith blender channelA channelB


--blendRGB :: (A.Elt a, A.IsFloating a) => Image a -> Image a -> (Exp a -> Exp a -> Exp a) -> Either Image.Error (Image a)
--blendRGB img1 img2 blender = do
--    r1 <- Image.lookup "r" img1
--    g1 <- Image.lookup "g" img1
--    b1 <- Image.lookup "b" img1
--    a1 <- Image.lookup "a" img1
--    r2 <- Image.lookup "r" img2
--    g2 <- Image.lookup "g" img2
--    b2 <- Image.lookup "b" img2
--    a2 <- Image.lookup "a" img2
--    let outimg = Image.insert "r" r'
--               $ Image.insert "g" g'
--               $ Image.insert "b" b'
--               $ Image.insert "a" a'
--               $ mempty
--        r'     = blendC r1 r2 blender
--        g'     = blendC g1 g2 blender
--        b'     = blendC b1 b2 blender
--        a'     = blendC a1 a2 blender
--    return outimg

blendAlpha :: (A.Elt a, A.IsFloating a) => Image a -> Image a -> Either Image.Error (Image a)
blendAlpha img1 img2 = do
    a2 <- Image.lookup "a" img2
    outimg <- blendAlpha' img1 img2 a2
    return outimg

blendAlpha' :: (A.Elt a, A.IsFloating a) => Image a -> Image a -> Channel a -> Either Image.Error (Image a)
blendAlpha' img1 img2 a = do
    r1 <- Image.lookup "r" img1
    g1 <- Image.lookup "g" img1
    b1 <- Image.lookup "b" img1
    r2 <- Image.lookup "r" img2
    g2 <- Image.lookup "g" img2
    b2 <- Image.lookup "b" img2
    let outimg = Image.insert "r" (blend r1 r2)
               $ Image.insert "g" (blend g1 g2)
               $ Image.insert "b" (blend b1 b2)
               $ img1
        blend = Channel.zipWith3 blenderAlpha a
    return outimg

--blendAlphaF =

-- #define ChannelBlend_Normal(A,B)     ((uint8)(A))
blenderNormal :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderNormal a _ = a

-- #define ChannelBlend_Lighten(A,B)    ((uint8)((B > A) ? B:A))
blenderLighten :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderLighten a b = b A.>* a A.? (b,a)

-- #define ChannelBlend_Darken(A,B)     ((uint8)((B > A) ? A:B))
blenderDarken :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderDarken a b = b A.>* a A.? (a,b)

-- #define ChannelBlend_Multiply(A,B)   ((uint8)((A * B) / 255))
blenderMultiply :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderMultiply a b = (a * b)

-- #define ChannelBlend_Average(A,B)    ((uint8)((A + B) / 2))
blenderAverage :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderAverage a b = (a + b) / 2

-- #define ChannelBlend_Add(A,B)        ((uint8)(min(255, (A + B))))
blenderAdd :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderAdd a b = 1 A.<* (a + b) A.? (1 , a + b)

-- #define ChannelBlend_Subtract(A,B)   ((uint8)((A + B < 255) ? 0:(A + B - 255)))
blenderSubtract :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderSubtract a b = (a + b) A.<* 1 A.? (0 , a + b - 1)

-- #define ChannelBlend_Difference(A,B) ((uint8)(abs(A - B)))
blenderDifference :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderDifference a b = a A.>* b A.? (a - b , b - a)

-- #define ChannelBlend_Negation(A,B)   ((uint8)(255 - abs(255 - A - B)))
blenderNegation :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderNegation a b = 1 - (1 - a - b A.>* 0 A.? (1 - a - b , -(1 - a - b)))

-- #define ChannelBlend_Screen(A,B)     ((uint8)(255 - (((255 - A) * (255 - B)) >> 8)))
blenderScreen :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderScreen a b = 1 - (1 - a) * (1 - b)

-- #define ChannelBlend_Exclusion(A,B)  ((uint8)(A + B - 2 * A * B / 255))
blenderExclusion :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderExclusion a b = a + b - 2 * a * b

-- #define ChannelBlend_Overlay(A,B)    ((uint8)((B < 128) ? (2 * A * B / 255):(255 - 2 * (255 - A) * (255 - B) / 255)))
blenderOverlay :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderOverlay a b = b A.<* 0.5 A.? ((2 * a * b) , (1 - 2 * (1 - a) * (1 - b)))

-- #define ChannelBlend_SoftLight(A,B)  ((uint8)((B < 128)?(2*((A>>1)+64))*((float)B/255):(255-(2*(255-((A>>1)+64))*(float)(255-B)/255))))
blenderSoftLight :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderSoftLight a b = b A.<* 0.5 A.? (2 * (a / 2 + 0.25) * b , 1 - 2 * (1 - (a / 2 + 0.25)) * (1-b))

-- #define ChannelBlend_HardLight(A,B)  (ChannelBlend_Overlay(B,A))
blenderHardLight :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderHardLight = flip blenderOverlay

-- #define ChannelBlend_ColorDodge(A,B) ((uint8)((B == 255) ? B:min(255, ((A << 8 ) / (255 - B)))))
blenderColorDodge :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderColorDodge a b = b A.>=* 1 A.? (b , 1 A.<* a / (1 - b) A.? (1 , a / (1 - b)))

-- #define ChannelBlend_ColorBurn(A,B)  ((uint8)((B == 0) ? B:max(0, (255 - ((255 - A) << 8 ) / B))))
blenderColorBurn :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderColorBurn a b = b A.<=* 0 A.? (b , 0 A.>* 1 - (1 - a) / b A.? (0 , 1 - (1 - a) / b))

-- #define ChannelBlend_LinearDodge(A,B)(ChannelBlend_Add(A,B))
blenderLinearDodge :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderLinearDodge = blenderAdd

-- #define ChannelBlend_LinearBurn(A,B) (ChannelBlend_Subtract(A,B))
blenderLinearBurn :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderLinearBurn = blenderSubtract

-- #define ChannelBlend_LinearLight(A,B)((uint8)(B < 128)?ChannelBlend_LinearBurn(A,(2 * B)):ChannelBlend_LinearDodge(A,(2 * (B - 128))))
blenderLinearLight :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderLinearLight a b = b A.<* 0.5 A.? (blenderLinearBurn a (2 * b) , blenderLinearDodge a (2 * (b - 0.5)))

-- #define ChannelBlend_VividLight(A,B) ((uint8)(B < 128)?ChannelBlend_ColorBurn(A,(2 * B)):ChannelBlend_ColorDodge(A,(2 * (B - 128))))
blenderVividLight :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderVividLight a b = b A.<* 0.5 A.? (blenderColorBurn a (2 * b) , blenderColorDodge a (2 * (b - 0.5)))

-- #define ChannelBlend_PinLight(A,B)   ((uint8)(B < 128)?ChannelBlend_Darken(A,(2 * B)):ChannelBlend_Lighten(A,(2 * (B - 128))))
blenderPinLight :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderPinLight a b = b A.<* 0.5 A.? (blenderDarken a (2 * b) , blenderLighten a (2 * (b - 0.5)))

-- #define ChannelBlend_HardMix(A,B)    ((uint8)((ChannelBlend_VividLight(A,B) < 128) ? 0:255))
blenderHardMix :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderHardMix a b = blenderVividLight a b A.<* 0.5 A.? (0 , 1)

-- #define ChannelBlend_Reflect(A,B)    ((uint8)((B == 255) ? B:min(255, (A * A / (255 - B)))))
blenderReflect :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderReflect a b = b A.>=* 1 A.? (b , 1 A.<* a * a / (1 - b) A.? (1 , a * a / (1 - b)))

-- #define ChannelBlend_Glow(A,B)       (ChannelBlend_Reflect(B,A))
blenderGlow :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderGlow = flip blenderReflect

-- #define ChannelBlend_Phoenix(A,B)    ((uint8)(min(A,B) - max(A,B) + 255))
blenderPhoenix :: (A.Elt a, A.IsFloating a) => Exp a -> Exp a -> Exp a
blenderPhoenix a b = (a A.<* b A.? (a , b)) - (a A.>* b A.? (a , b)) + 1

-- #define ChannelBlend_Alpha(A,B,O)    ((uint8)(O * A + (1 - O) * B))
blenderAlpha :: Num a => a -> a -> a -> a
blenderAlpha o a b = (o * a + (1 - o) * b)

-- #define ChannelBlend_AlphaF(A,B,F,O) (ChannelBlend_Alpha(F(A,B),A,O))
blenderAlphaF :: Num a => (a -> t -> a) -> a -> a -> t -> a
blenderAlphaF f o a b = blenderAlpha (f a b) a o



--- keying

keyColor :: (A.Elt a, A.IsFloating a) => String3 -> Exp3 a -> Exp3 a
            -> (Exp a -> Exp a) -> Image a -> Either Image.Error (Image a)
keyColor (nameA, nameB, nameC) (epsA, epsB, epsC) (valA, valB, valC) f img = do
  channelA <- Image.lookup nameA img
  channelB <- Image.lookup nameB img
  channelC <- Image.lookup nameC img
  let outimg = Image.insert nameA (match channelA)
             $ Image.insert nameB (match channelB)
             $ Image.insert nameC (match channelC)
             $ img
      match c = Channel.zipWith (\a b -> b A.? (f a , a)) c matched
      matched = Channel.zipWith3 (\a b c -> (epsilon a valA epsA) A.&&* (epsilon b valB epsB) A.&&* (epsilon c valC epsC)) channelA channelB channelC
  return outimg



-- extracting background

--generateHistList result [] = result
--generateHistList result@((x,y):xs) (val:arr) =
--  if val A.==* x
--    then generateHistList arr ([(x,y+1)] ++ xs)
--    else generateHistList arr ([(val,1)] ++ result)

--findMostFrequent eps arr = findMostFrequent' [] eps arr
--findMostFrequent arr = arr !! 0

--findMostFrequent' result _ [] = result
--findMostFrequent' result eps arr@((x,_):xs) = findMostFrequent (val A.>* result A.? (val,result)) eps xs
--  where val = step 0 arr
--        step res [] = res
--        step res ((a,b):rest) =

extractBackground :: (A.Elt a, A.IsFloating a) => String3 -> [Image a] -> Either Image.Error (Image a)
extractBackground (nameA, nameB, nameC) images = do
  let firstImg    = images !! 0
  channelsA <- sequence $ fmap (Image.lookup nameA) images
  channelsB <- sequence $ fmap (Image.lookup nameB) images
  channelsC <- sequence $ fmap (Image.lookup nameC) images
  tmpChannel <- Image.lookup "a" firstImg
  let outimg = Image.insert nameA channelA'
             $ Image.insert nameB channelB'
             $ Image.insert nameC channelC'
             $ Image.insert "a" tmpChannel
             $ mempty
      exampleChannel = channelsA !! 0
      channelShape = Channel.shape exampleChannel
      channelA' = Channel.generate channelShape (getMostFreq channelsA)
      channelB' = Channel.generate channelShape (getMostFreq channelsB)
      channelC' = Channel.generate channelShape (getMostFreq channelsC)
      getMostFreq channels ix = let
                                (A.Z A.:. i A.:. j) = A.unlift ix
                                --empty = A.use $ A.fromList (A.Z A.:. 0) []
                                --folder x acc = acc A.++ (A.flatten $ A.unit x)
                                --result = median' $ bsort' $ foldr folder empty $ fmap ((flip (Channel.at)) (A.index2 i j)) channels
                              in
                                --findMostFrequent $ (generateHistList []) $ bsort (fmap ((flip (Channel.at)) (A.index2 i j)) channels)
                                median $ bsort (fmap ((flip (Channel.at)) (A.index2 i j)) channels)
  return outimg



-- cut out from background

cutOut :: (A.Elt a, A.IsFloating a) => String3 -> Exp3 a -> (Exp a -> Exp a)
          -> Image a -> Image a -> Either Image.Error (Image a)
cutOut (nameA, nameB, nameC) (epsA, epsB, epsC) f imgIn imgBackground = do
  channelA1 <- Image.lookup nameA imgIn
  channelB1 <- Image.lookup nameB imgIn
  channelC1 <- Image.lookup nameC imgIn
  channelA2 <- Image.lookup nameA imgBackground
  channelB2 <- Image.lookup nameB imgBackground
  channelC2 <- Image.lookup nameC imgBackground
  let outimg = Image.insert nameA (match channelA1)
             $ Image.insert nameB (match channelB1)
             $ Image.insert nameC (match channelC1)
             $ imgIn
      match cha = Channel.zipWith (\a b -> b A.? (f a , a)) cha matched
      matched   = Channel.zipWith3 (\a b c -> (epsilon' a epsA) A.&&* (epsilon' b epsB) A.&&* (epsilon' c epsC)) epsilonsA epsilonsB epsilonsC
      epsilonsA = Channel.zipWith (-) channelA1 channelA2
      epsilonsB = Channel.zipWith (-) channelB1 channelB2
      epsilonsC = Channel.zipWith (-) channelC1 channelC2
  return outimg
