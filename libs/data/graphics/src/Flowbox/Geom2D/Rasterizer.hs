---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
-- {-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}

module Flowbox.Geom2D.Rasterizer (
    module Flowbox.Geom2D.Rasterizer,
    Point2(..)
) where

import           Data.Array.Accelerate           ((&&*), (==*), (>*), (<*), (||*))
import qualified Data.Array.Accelerate           as A
import           Data.Array.Accelerate.IO
import           Data.Bits                       ((.&.))
import           Data.Maybe
import           Data.VectorSpace
import qualified Data.Vector.Unboxed as VU
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude                hiding (Path, (<*))
import           Diagrams.Segment
import           Foreign.Storable
import           Graphics.Rendering.Cairo        hiding (Path, translate)
import           Math.Space.Space                (Grid (..))
import           System.IO.Unsafe

import           Flowbox.Geom2D.Accelerate.QuadraticBezier.Solve
import           Flowbox.Geom2D.ControlPoint
import           Flowbox.Geom2D.CubicBezier
import           Flowbox.Geom2D.Mask
import           Flowbox.Geom2D.Path
import           Flowbox.Geom2D.QuadraticBezier
import           Flowbox.Geom2D.QuadraticBezier.Conversion
import qualified Flowbox.Graphics.Composition.Filter             as Filter
import qualified Flowbox.Graphics.Image.Channel                  as Channel
import           Flowbox.Graphics.Image.Image                    (Image)
import qualified Flowbox.Graphics.Image.Image                    as Image
import           Flowbox.Graphics.Image.IO.BMP
import qualified Flowbox.Graphics.Image.View                     as View
import           Flowbox.Graphics.Mockup.Basic
import qualified Flowbox.Graphics.Shader.Pipe                    as Shader
import           Flowbox.Graphics.Shader.Rasterizer
import           Flowbox.Graphics.Shader.Matrix
import           Flowbox.Graphics.Utils.Accelerate               (variable)
import           Flowbox.Math.Matrix                             ((:.) (..), DIM2, Matrix (..), Matrix2, Z (..))
import qualified Flowbox.Math.Matrix                             as M
import           Flowbox.Math.Index

import           Flowbox.Prelude                                 hiding (use, ( # ), (<*))
import           Math.Coordinate.Cartesian                       (Point2 (..))


import qualified Debug.Trace as Dbg (trace)


-- intended to be hidden from this package
f2d :: Real a => a -> Double
f2d = fromRational . toRational

f2f :: Real a => a -> Float
f2f = fromRational . toRational

-- TODO[1]: revert to this version when the wrapping model for handling GUI's use-case gets implemented
--unpackP :: Num a => Maybe (Point2 a) -> Point2 a
--unpackP = fromMaybe (Point2 0 0)
unpackP :: Fractional a => Point2 a -> Point2 a -> Maybe (Point2 a) -> Point2 a
unpackP a b = fromMaybe ((b - a)/3)

-- TODO[1]
--makeSegments :: Real a => Bool -> [ControlPoint a] -> [Segment Closed R2]
--makeSegments closed points = combine points
--    where combine []  = []
--          combine [a'] = if not closed then [] else let
--                  ControlPoint (Point2 ax ay) _ b' = f2d' a'
--                  ControlPoint (Point2 dx dy) c' _ = f2d' $ head points
--                  Point2 bx by = unpackP b'
--                  Point2 cx cy = unpackP c'
--                  a = r2 (ax , ay)
--                  b = r2 (bx , by)
--                  c = r2 (cx , cy)
--                  d = r2 (dx , dy)
--              in [bezier3 b (d ^+^ c ^-^ a) (d ^-^ a)]
--          combine (a':d':xs) = let
--                  ControlPoint (Point2 ax ay) _ b' = f2d' a'
--                  ControlPoint (Point2 dx dy) c' _ = f2d' d'
--                  Point2 bx by = unpackP b'
--                  Point2 cx cy = unpackP c'
--                  a = r2 (ax , ay)
--                  b = r2 (bx , by)
--                  c = r2 (cx , cy)
--                  d = r2 (dx , dy)
--              in bezier3 b (d ^+^ c ^-^ a) (d ^-^ a) : combine (d':xs)
--          combine _ = error "Flowbox.Geom2D.Rasterizer.makeSegments: unsupported ammount of points"
--          f2d' = fmap f2d
makeSegments :: (Real a, Fractional a) => Bool -> [ControlPoint a] -> [Segment Closed R2]
makeSegments closed points = combine points
    where combine []  = []
          combine [a'] = if not closed then [] else let
                  ControlPoint pa@(Point2 ax ay) _ b' = f2d' a'
                  ControlPoint pd@(Point2 dx dy) c' _ = f2d' $ head points
                  Point2 bx by = unpackP pa pd b'
                  Point2 cx cy = unpackP pd pa c'
                  a = r2 (ax , ay)
                  b = r2 (bx , by)
                  c = r2 (cx , cy)
                  d = r2 (dx , dy)
              in [bezier3 b (d ^+^ c ^-^ a) (d ^-^ a)]
          combine (a':d':xs) = let
                  ControlPoint pa@(Point2 ax ay) _ b' = f2d' a'
                  ControlPoint pd@(Point2 dx dy) c' _ = f2d' d'
                  Point2 bx by = unpackP pa pd b'
                  Point2 cx cy = unpackP pd pa c'
                  a = r2 (ax , ay)
                  b = r2 (bx , by)
                  c = r2 (cx , cy)
                  d = r2 (dx , dy)
              in bezier3 b (d ^+^ c ^-^ a) (d ^-^ a) : combine (d':xs)
          combine _ = error "Flowbox.Geom2D.Rasterizer.makeSegments: unsupported ammount of points"
          f2d' = fmap f2d

-- TODO[1]
--makeCubics :: Real a => Path a -> [CubicBezier a]
--makeCubics (Path closed points) = combine points
--    where combine [] = []
--          combine [a'] = if not closed then [] else let
--                  ControlPoint a _ (unpackP -> b) = a'
--                  ControlPoint d (unpackP -> c) _ = head points
--              in [CubicBezier a (a+b) (d+c) d]
--          combine (a':d':xs) = let
--                  ControlPoint a _ (unpackP -> b) = a'
--                  ControlPoint d (unpackP -> c) _ = d'
--              in CubicBezier a (a+b) (d+c) d : combine (d':xs)
--          combine _ = error "Flowbox.Geom2D.Rasterizer.makeCubics: unsupported ammount of points"
makeCubics :: (Real a, Fractional a) => Path a -> [CubicBezier a]
makeCubics (Path closed points) = combine points
    where combine [] = []
          combine [a'] = if not closed then [] else let
                  ControlPoint a _ b' = a'
                  ControlPoint d c' _ = head points
                  b = unpackP a d b'
                  c = unpackP d a c'
              in [CubicBezier a (a+b) (d+c) d]
          combine (a':d':xs) = let
                  ControlPoint a _ b' = a'
                  ControlPoint d c' _ = d'
                  b = unpackP a d b'
                  c = unpackP d a c'
              in CubicBezier a (a+b) (d+c) d : combine (d':xs)
          combine _ = error "Flowbox.Geom2D.Rasterizer.makeCubics: unsupported ammount of points"

pathToRGBA32 :: (Real a, Fractional a) => Int -> Int -> Path a -> A.Array DIM2 RGBA32
pathToRGBA32 w h (Path _ []) = A.fromList (Z:.h:.w) [0..]
pathToRGBA32 w h (Path closed points) = unsafePerformIO rasterize
    where ControlPoint (Point2 ox oy) _ _ = fmap f2d $ head points
          h' = fromIntegral h
          rasterize = do
              let path = fromSegments $ makeSegments closed points
                  diagram = case closed of
                      False -> path                        # translate (r2 (ox,oy)) # scaleY (-1) # translateY h' # lc white # lw (Output 1) :: Diagram Cairo R2
                      True  -> (strokeLoop.closeLine) path # translate (r2 (ox,oy)) # scaleY (-1) # translateY h' # fc white # lw (Output 0) :: Diagram Cairo R2
                  (_, r) = renderDia Cairo (CairoOptions "" (Dims (fromIntegral w) (fromIntegral h)) RenderOnly True) (diagram :: Diagram Cairo R2)
              surface <- createImageSurface FormatARGB32 w h
              renderWith surface r
              bs <- imageSurfaceGetData surface
              fromByteString (Z:.h:.w) ((), bs)

pathToMatrix :: (Real a, Fractional a) => Int -> Int -> Path a -> Matrix2 Float
pathToMatrix w h path = extractArr $ pathToRGBA32 w h path
    where extractArr arr = Delayed $ A.map extractVal $ A.use arr
          extractVal :: M.Exp RGBA32 -> M.Exp Float
          extractVal rgba = (A.fromIntegral $ (rgba `div` 0x1000000) .&. 0xFF) / 255

translateFeather :: Num a => Path a -> Path a -> Path a
translateFeather (Path _ path) (Path isClosed feather) = Path isClosed $ zipWith process path feather
    where process (ControlPoint p _ _) (ControlPoint f hIn hOut) = ControlPoint (p+f) hIn hOut

rasterizeMask :: (Real a, Fractional a) => Int -> Int -> Mask a -> Matrix2 Float
rasterizeMask w h (Mask pathRaw maybeFeather) =
    case maybeFeather of
        Nothing -> process path
        Just featherRaw -> process $ checkEqual (translateFeather pathRaw featherRaw) pathRaw
    where hmat :: (A.Elt e, A.IsFloating e) => Matrix2 e
          hmat = id M.>-> Filter.normalize $ Filter.toMatrix (Grid 1 kernelSize) $ Filter.gauss 1.0
          vmat :: (A.Elt e, A.IsFloating e) => Matrix2 e
          vmat = id M.>-> Filter.normalize $ Filter.toMatrix (Grid kernelSize 1) $ Filter.gauss 1.0
          kernelSize = 4 -- magick number
          p = Shader.pipe A.Clamp
          process x = rasterizer $ id `p` Filter.filter 1 vmat `p` Filter.filter 1 hmat `p` id $ fromMatrix A.Clamp x
          ptm = pathToMatrix w h
          path = ptm pathRaw
          distance p (A.unlift . A.unindex2 -> (A.fromIntegral -> y, A.fromIntegral -> x) :: (A.Exp Int, A.Exp Int)) = let
                  h' = A.fromIntegral $ A.lift h
                  d = distanceFromQuadratics' (A.lift $ Point2 x ((y - (h'/2))*(-1) + (h'/2)))
                  dp = d p
              in dp
          combine p f dp df = let
                  f0 = f <* 0.01 :: M.Exp Bool
                  f1 = f >* 0.99 :: M.Exp Bool
                  p0 = p <* 0.01 :: M.Exp Bool
                  p1 = p >* 0.99 :: M.Exp Bool
                  p01 = (A.not p1) &&* (A.not p0)
                  cond1 = (p0 &&* (A.not f1)) ||* (p1 &&* f1) ||* (p01 &&* f0)
                  cond2 = (p0 &&* f1) ||* (p1 &&* f0)
                  c2 = A.fromIntegral $ A.boolToInt cond2
                  nc2 = A.fromIntegral $ A.boolToInt $ A.not cond2
                  x' = ((dp*p + df*f)/(dp+df))
                  --x' = (sigmoid (((df*f + dp*p)/(dp*f + df*p))*6 - 5.5))
              in A.cond (cond1)
                  (p*f)
                  (c2*x' + nc2*(max p f))
          sigmoid a = 1 / (1 + (2.71828**(-a))) -- 1 / (1 +  2.71828^(-a))
          --srgb v = v A.<=* 0.0031308 A.? (12.92 * v, (1.055 * v) ** (1 / 2.4) - 0.055)
          checkEqual fea pat
              | fea == pat = path
              | otherwise  = let
                      feather = ptm fea
                      convert p = let
                              a = makeCubics p
                              quads = convertCubicsToQuadratics 5 0.001 $ (fmap.fmap) f2f a
                          in {-Dbg.trace ("calling with quads = " ++ show quads)-} (A.use $ A.fromList (Z :. length quads) quads)
                      cA = convert pathRaw
                      cB = convert fea
                      dP = M.generate (A.index2 (variable h) (variable w)) $ distance cA
                      dF = M.generate (A.index2 (variable h) (variable w)) $ distance cB
                  in M.zipWith4 (\p f dp df -> (combine p f dp df)) path feather dP dF

rasterizeMaskL :: (Real a, Fractional a) => Int -> Int -> Mask a -> Matrix2 Float
rasterizeMaskL w h mask@(Mask pathRaw maybeFeather) = 
    case maybeFeather of
        Nothing -> ptm pathRaw
        Just featherRaw -> unsafePerformIO $ M.mutableProcess temporaryBackend (drawLines w h pathRaw featherRaw) (ptm pathRaw)
    where
      ptm = pathToMatrix w h

drawLines :: (Storable a, RealFrac a, Real b, Fractional b) => Int -> Int -> Path b -> Path b -> M.MImage a -> IO ()
drawLines width height path feather img = do
    let array x y     = boundedIndex2D A.Clamp img $ Point2 x y
        pathCubics    = makeCubics path
        featherCubics = makeCubics feather
    return ()
    goThroughSegments pathCubics featherCubics array height

goThroughSegments [] [] _ _ = return ()
goThroughSegments (p:ax) (f:fx) array h = do
    func array p f h
    goThroughSegments ax fx array h

cubic' t p0 p1 p2 p3 = ((1.0 - t)**3)*p0 + 3*((1 - t)**2)*t*p1 + 3*(1 - t)*t*t*p2 + (t**3)*p3

func array pBezier fBezier h = do
    let CubicBezier pC0 pC1 pC2 pC3 = pBezier
        CubicBezier fC0 fC1 fC2 fC3 = fBezier
        Point2 pC0x pC0y' = pC0
        Point2 pC1x pC1y' = pC1
        Point2 pC2x pC2y' = pC2
        Point2 pC3x pC3y' = pC3
        Point2 fC0x' fC0y' = fC0
        Point2 fC1x' fC1y' = fC1
        Point2 fC2x' fC2y' = fC2
        Point2 fC3x' fC3y' = fC3

        pC0y = ft $ flipy pC0y'
        pC1y = ft $ flipy pC1y'
        pC2y = ft $ flipy pC2y'
        pC3y = ft $ flipy pC3y'

        fC0x = fC0x' + pC0x
        fC0y = ft $ (flipy (fC0y' + pC0y'))
        fC1x = fC1x' + pC0x
        fC1y = ft $ (flipy (fC1y'+ pC0y'))
        fC2x = fC2x' + pC3x
        fC2y = ft $ (flipy (fC2y' + pC3y'))
        fC3x = fC3x' + pC3x
        fC3y = ft $ (flipy (fC3y' + pC3y'))
        
        h' = fromIntegral h
        flipy x = ((x - (h'/2)) * (-1)) + (h'/2)
        ft x = fromRational (toRational x)
        
        pC0xft = ft pC0x
        pC1xft = ft pC1x
        pC2xft = ft pC2x
        pC3xft = ft pC3x
        fC0xft = ft fC0x
        fC1xft = ft fC1x
        fC2xft = ft fC2x
        fC3xft = ft fC3x

        lab = sqrt $ (abs (pC0xft - pC1xft))**2 + (abs (pC0y - pC1y))**2
        lbc = sqrt $ (abs (pC1xft - pC2xft))**2 + (abs (pC1y - pC2y))**2
        lcd = sqrt $ (abs (pC2xft - pC3xft))**2 + (abs (pC2y - pC3y))**2
        l1   = lab + lbc + lcd
        lab2 = sqrt $ (abs (fC0xft - fC1xft))**2 + (abs (fC0y - fC1y))**2
        lbc2 = sqrt $ (abs (fC1xft - fC2xft))**2 + (abs (fC1y - fC2y))**2
        lcd2 = sqrt $ (abs (fC2xft - fC3xft))**2 + (abs (fC2y - fC3y))**2
        l2   = lab2 + lbc2 + lcd2
        l    = (max l1 l2) * 1.7
        intl = ceiling l

    VU.forM_ (VU.generate intl id) $ \t' ->
        let t = (fromIntegral t') / l
        in lineFunc array (cubic' t pC0xft pC1xft pC2xft pC3xft) (cubic' t pC0y pC1y pC2y pC3y) (cubic' t fC0xft fC1xft fC2xft fC3xft) (cubic' t fC0y fC1y fC2y fC3y)

lineFunc array x1' y1' x2' y2' = do
    let vecxraw = x2' - x1'
        vecyraw = y2' - y1'
        l       = sqrt $ (vecxraw)**2 + (vecyraw)**2
        ftl     = fromRational (toRational l)
        intl    = ceiling l
    VU.forM_ (VU.generate intl id) $ \y ->
        let vecx    = (t / l) * vecxraw
            vecy    = (t / l) * vecyraw
            t       = fromIntegral y
            newvecx = ceiling $ vecx + x1'
            newvecy = ceiling $ vecy + y1'
        in array newvecx newvecy M.$= 1.0 - (t / ftl)


matrixToImage :: Matrix2 Float -> Image
matrixToImage a = Image.singleton view
    where view = View.append (Channel.ChannelFloat "rgba.r" $ Channel.MatrixData w)
               $ View.append (Channel.ChannelFloat "rgba.g" $ Channel.MatrixData w)
               $ View.append (Channel.ChannelFloat "rgba.b" $ Channel.MatrixData w)
               $ View.append (Channel.ChannelFloat "rgba.a" $ Channel.MatrixData a)
               $ View.empty "rgba"
          w = M.map (\_ -> 1) a
