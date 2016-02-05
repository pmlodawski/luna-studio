{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.GuiMockup.LineFit
    (
      CubicBezier(..)
    , Openness(..)
    , generateBezier
    , fitCurve
    , fitCurve'
    , chordLengthParameterize
    , reparameterize
--    , test
    , test2
--    , test3
	) where


import           GHC.Generics                 (Generic)

import           Control.Applicative          ((<$>), (<*>))
import           Control.Error                hiding (err)
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad                (forM_, when)
import           Control.Monad.Loops
import           Control.Monad.ST             (ST, runST)
import           Control.Monad.Trans.Class    (lift)
import           Data.Binary
import           Data.List                    (intercalate)
import           Data.STRef
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.Storable.Tuple       ()
import           Linear                       hiding (point, trace)

import           Flowbox.GuiMockup.JSInterop

import           Control.Exception            as Exc
import           System.IO.Unsafe



toVec :: CubicBezier Double -> V.Vector (V2 Double)
toVec (CubicBezier c0 c1 c2 c3) = V.fromList [c0, c1, c2, c3]

data Openness = Open | Closed
	deriving (Show, Generic)

instance Binary Openness

type ControlPoint = (V2 Double, V2 Double, V2 Double)

-- main function in this module

fitCurve' :: [V2 Double] -> Double -> Openness -> [CubicBezier Double]
fitCurve' points err openness = case openness of
    Open   -> V.toList $ fitCubic points' tHat1 tHat2 err
    Closed -> (V.head $ fitCubic (V.fromList [head points, last points]) tHat1 tHat2 err) : fitCurve' points err Open
    where
        points' = V.fromList points
        len = V.length points'
        tHat1 = computeLeftTangent points' 0
        tHat2 = computeRightTangent points' $ len - 1

fitCurve :: [V2 Double] -> Double -> Openness -> [ControlPoint]
fitCurve a b c = unsafePerformIO $ Exc.catch (return $ fitCurve'' a b c) (\(Exc.ErrorCall s) -> putStrLn s >> return [])

fitCurve'' :: [V2 Double] -> Double -> Openness -> [ControlPoint]
fitCurve'' points err openness = case openness of
    Open   -> beziersToControlPoints $ V.toList $ fitCubic points' tHat1 tHat2 err
    Closed -> let controlPoints = fitCurve points err Open
                  firstControlPoint@(fstCP, fstCPHi, fstCPHo) = head controlPoints
                  penultimateControlPoint@(pCP, pCPHi, pCPHo) = if length controlPoints == 2 then last controlPoints else (last.init) controlPoints
                                                                -- ^^^^^^^^^^^^^^^^^^^^^^^^^ [MM]: when we have only 2 control points, taking the last one as penultimate is kinda ok
                  linkingBezier = V.head $ fitCubic (V.fromList [pCP, fstCP]) (negated $ pCPHi - pCP) (negated tHat1) err

                  firstControlPointModified = firstControlPoint & _2 .~ fstCPHi
                  lastControlPointModified = penultimateControlPoint & _3 .~ pCPHo
                  middleControlPoints = if length controlPoints == 2 then [] else (init . init . tail) controlPoints
              in firstControlPointModified : middleControlPoints ++ [lastControlPointModified]

    where
        points' = V.fromList points
        len = V.length points'
        tHat1 = computeLeftTangent points' 0
        tHat2 = computeRightTangent points' $ len - 1

beziersToFullControlPoints beziers = map snd $ tail . tail $ scanl (\(prevhi,_) (CubicBezier point1 ho hi point2) -> (hi,(point1,prevhi,ho))) (0.0, (firstPoint,dummyHandle,firstPointHo)) beziers where
    CubicBezier firstPoint firstPointHo _ _ = head beziers
    dummyHandle = V2 0 0
    CubicBezier _ _ lastPointHi lastPoint = last beziers

beziersToControlPoints :: [CubicBezier Double] -> [ControlPoint]
beziersToControlPoints beziers = [startControlPoint] ++ midPoints ++ [endControlPoint]
    where
        midPoints = beziersToFullControlPoints beziers
        startControlPoint = (ps , ps, ho)
        endControlPoint = (pe, hi, pe)
        CubicBezier ps ho _ _ = head beziers
        CubicBezier _ _ hi pe = last beziers

fitCubic :: V.Vector (V2 Double) -> V2 Double -> V2 Double -> Double -> V.Vector (CubicBezier Double)
fitCubic points tHat1 tHat2 err
    | V.length points == 2 =
        let c0 = points V.! 0
            c3 = points V.! 1

            dist = distance (points V.! 1) (points V.! 0) / 3
            c1 = c0 + tHat1 ^* dist
            c2 = c3 + tHat2 ^* dist
        in V.singleton $ CubicBezier c0 c1 c2 c3
    | otherwise = if maxError < err
                    then V.singleton bezCurve
                    else if maxError < iterationError
                           then case iteration of
                                  Left curve       -> V.singleton curve
                                  Right splitPoint -> split splitPoint
                           else split initialSplitPoint
        where
            u = chordLengthParameterize points
            bezCurve = generateBezier points u tHat1 tHat2
            (maxError, initialSplitPoint) = computeMaxError points bezCurve u
            iterationError = max err $ err ** 2

            iteration = runST $ runEitherT $ do
                u' <- lift $ newSTRef u
                splitPoint' <- lift $ newSTRef initialSplitPoint
                bezCurve' <- lift $ newSTRef bezCurve

                forM_ [(1::Int)..4] $ \_ -> do
                    uVal <- lift $ readSTRef u'
                    bezCurveVal <- lift $ readSTRef bezCurve'
                    let uPrime = reparameterize points uVal bezCurveVal

                    lift $ writeSTRef bezCurve' $ generateBezier points uPrime tHat1 tHat2

                    bezCurveVal <- lift $ readSTRef bezCurve'
                    let (maxErrorVal, splitPointVal) = computeMaxError points bezCurveVal uPrime

                    lift $ writeSTRef splitPoint' splitPointVal
                    when (maxErrorVal < iterationError) $ left bezCurveVal

                    lift $ writeSTRef u' uPrime

                lift (readSTRef splitPoint') >>= right

            split splitPoint = leftCurve V.++ rightCurve
                where
                    tHatCenter = computeCenterTangent points splitPoint
                    (left, right) = splitPoints splitPoint points
                    leftCurve = fitCubic left tHat1 tHatCenter err
                    rightCurve = fitCubic right (negated tHatCenter) tHat2 err

splitPoints :: (Storable a) => Int -> V.Vector a -> (V.Vector a, V.Vector a)
splitPoints ix vec = (V.snoc left $ V.head right, right)
    where
        (left, right) = V.splitAt ix vec

reparameterize :: V.Vector (V2 Double) -> V.Vector Double -> CubicBezier Double -> V.Vector Double
reparameterize points u bezierCurve = V.create $ do
    let len = V.length points
    uPrime <- MV.new len

    forM_ [0 .. len - 1] $ \i -> do
        let point = points V.! i
            ui    = u V.! i
        MV.write uPrime i $ newtonRaphsonRootFind bezierCurve point ui

    return uPrime

newtonRaphsonRootFind :: CubicBezier Double -> V2 Double -> Double -> Double
newtonRaphsonRootFind q p u =
    if denominator == 0 then u else u - numerator / denominator
    where
        qu = evalBezier qVector u
        qVector = toVec q
        q1 = V.create $ do
            q1' <- MV.new 3

            forM_ [0..2] $ \i -> do
                MV.write q1' i $ (qVector V.! (i + 1) - qVector V.! i) * 3

            return q1'

        q2 = V.create $ do
            q2' <- MV.new 2

            forM_ [0..1] $ \i -> do
                MV.write q2' i $ (q1 V.! (i + 1) - q1 V.! i) * 2

            return q2'

        q1u = evalBezier q1 u
        q2u = evalBezier q2 u

        numerator = (qu ^. _x - p ^. _x) * q1u ^. _x + (qu ^. _y - p ^. _y) * q1u ^. _y
        denominator = (q1u ^. _x) ** 2 + (q1u ^. _y) ** 2 +
                      (qu ^. _x - p ^. _x) * q2u ^. _x + (qu ^. _y - p ^. _y) * q2u ^. _y


generateBezier :: V.Vector (V2 Double) -> V.Vector Double -> V2 Double -> V2 Double -> CubicBezier Double
generateBezier points uPrime tHat1 tHat2 =
    if alphaL < eps || alphaR < eps
        then let dist = segLength / 3
                 c1 = c0 + tHat1 ^* dist
                 c2 = c3 + tHat2 ^* dist
             in CubicBezier c0 c1 c2 c3
        else let c1 = c0 + tHat1 ^* alphaL
                 c2 = c3 + tHat2 ^* alphaR
             in CubicBezier c0 c1 c2 c3
    where
        c0 = V.head points
        c3 = V.last points

        a = computeAMatrix (V.length points) uPrime tHat1 tHat2
        c = computeC a
        x = computeX points a uPrime

        detC0C1 = det22 c
        detC0X  = det22 $ c & column _y .~ x
        detXC1  = det22 $ c & column _x .~ x

        alphaL = if detC0C1 == 0 then 0 else detXC1 / detC0C1
        alphaR = if detC0C1 == 0 then 0 else detC0X / detC0C1

        segLength = distance (V.last points) (V.head points)
        eps = 1.0e-6 * segLength

computeMaxError :: V.Vector (V2 Double) -> CubicBezier Double -> V.Vector Double -> (Double, Int)
computeMaxError points bezier u = runST $ do
    maxDist <- newSTRef 0
    let len = V.length points
    splitPoint <- newSTRef $ len `div` 2

    forM_ [1 .. len - 1] $ \i -> do
        let p = evalBezier (toVec bezier) $ u V.! i
            v = p - (points V.! i)
            dist = quadrance v

        maxDist' <- readSTRef maxDist
        when (dist >= maxDist') $ do
            writeSTRef maxDist dist
            writeSTRef splitPoint i

    maxDist' <- readSTRef maxDist
    splitPoint' <- readSTRef splitPoint

    return (maxDist' , splitPoint')

evalBezier :: V.Vector (V2 Double) -> Double -> V2 Double
evalBezier vtemp t = runST $ do
    let degree = V.length vtemp - 1
    v <- V.thaw vtemp

    forM_ [1..degree] $ \i -> do
        forM_ [0..degree-i] $ \j -> do
            vj <- MV.read v j
            vj1 <- MV.read v (j + 1)
            MV.write v j $ vj ^* (1 - t) + vj1 ^* t

    MV.read v 0


computeX :: V.Vector (V2 Double) -> V.Vector (V2 Double, V2 Double) -> V.Vector Double -> V2 Double
computeX points a uPrime = runST $ do
    x0 <- newSTRef 0
    x1 <- newSTRef 0

    let len = V.length points
    forM_ [0 .. len - 1] $ \i -> do
        let iPoint = points V.! i
            firstPoint = V.head points
            lastPoint = V.last points
            iUPrime = uPrime V.! i
            -- tmp = iPoint -
            --         firstPoint ^* b0 iUPrime +
            --           firstPoint ^* b1 iUPrime +
            --             lastPoint ^* b2 iUPrime + lastPoint ^* b3 iUPrime
            tmp = iPoint - firstPoint ^* (b0 iUPrime + b1 iUPrime)
                         - lastPoint ^* (b2 iUPrime + b3 iUPrime)
            ai0 = fst $ a V.! i
            ai1 = snd $ a V.! i

        modifySTRef' x0 (+ (ai0 `dot` tmp))
        modifySTRef' x1 (+ (ai1 `dot` tmp))

    x0' <- readSTRef x0
    x1' <- readSTRef x1

    return $ V2 x0' x1'

computeC :: V.Vector (V2 Double, V2 Double) -> M22 Double
computeC a = runST $ do
    c00 <- newSTRef 0
    c01 <- newSTRef 0
    c10 <- newSTRef 0
    c11 <- newSTRef 0

    let len = V.length a
    forM_ [0 .. len - 1] $ \i -> do
        let ai0 = fst $ a V.! i
            ai1 = snd $ a V.! i
        modifySTRef' c00 (+ (ai0 `dot` ai0))
        modifySTRef' c01 (+ (ai0 `dot` ai1))
        readSTRef c01 >>= writeSTRef c10
        modifySTRef' c11 (+ (ai1 `dot` ai1))

    c00' <- readSTRef c00
    c01' <- readSTRef c01
    c10' <- readSTRef c10
    c11' <- readSTRef c11

    return $ V2 (V2 c00' c01') (V2 c10' c11')

computeAMatrix :: Int -> V.Vector Double -> V2 Double -> V2 Double -> V.Vector (V2 Double, V2 Double)
computeAMatrix len uPrime tHat1 tHat2 = V.create $ do
    t <- MV.new len

    forM_ [0 .. len - 1] $ \i -> do
        let currentUPrime = uPrime V.! i
            x = tHat1 ^* b1 currentUPrime
            y = tHat2 ^* b2 currentUPrime
        MV.write t i (x, y)

    return t

b0, b1, b2, b3 :: Double -> Double
b0 u = tmp * tmp * tmp
    where
        tmp = 1 - u

b1 u = 3 * u * (tmp * tmp)
    where
        tmp = 1 - u

b2 u = 3 * u * u * tmp
    where
        tmp = 1 - u

b3 u = u * u * u

chordLengthParameterize :: V.Vector (V2 Double) -> V.Vector Double
chordLengthParameterize points = V.create $ do
    u <- MV.new $ V.length points
    MV.write u 0 0

    forM_ [1 .. V.length points - 1] $ \i -> do
        t <- MV.read u (i - 1)
        let dist = distance (points V.! i) (points V.! (i - 1))
        MV.write u i $ t + dist

    last' <- MV.read u (V.length points - 1)

    forM_ [1 .. V.length points - 1] $ \i -> do
        current <- MV.read u i
        MV.write u i $ current / last'

    return u

computeRightTangent :: V.Vector (V2 Double) -> Int -> V2 Double
computeRightTangent points end = tHat2
    where
        tHat2 = normalize tHat2'
        tHat2' = avgPoint - (points V.! end) --(points V.! end)
        avgPoint = (V.sum $ V.drop ((V.length points) - avgSample) points)/(fromIntegral avgSample)
        avgSample = min 5 $ V.length points -1


computeLeftTangent :: V.Vector (V2 Double) -> Int -> V2 Double
computeLeftTangent points end = tHat1
    where
        tHat1 = normalize tHat1'
        tHat1' = avgPoint - (points V.! end) --(points V.! end)
        avgPoint = (V.sum $ V.take avgSample points)/(fromIntegral avgSample)
        avgSample = min 5 $ V.length points -1

computeCenterTangent :: V.Vector (V2 Double) -> Int -> V2 Double
computeCenterTangent points center = normalize tHatCenter
    where
        v1 = (points V.! (center - 1)) - (points V.! center)
        v2 = (points V.! center) - (points V.! (center + 1))
        tHatCenter = (v1 + v2) ^/ 2

linearity :: (Num a, Floating a, Epsilon a) => V2 a -> V2 a -> V2 a -> a
linearity p0 p1 p2 = (a `dot` b) / n
    where
        a = p2 - p0
        b = p1 - p0
        n = norm a * norm b

test2 :: [[Double]] -> String
test2 input = jsify $ V.map (curry3 linearity) vector
    where
        inputVector = readPoints input
        vector = V.zipWith3 (,,) inputVector (V.tail inputVector) (V.tail $ V.tail inputVector)
        jsify = wrap . intercalate ", " . map show . V.toList
         where
             wrap s = "[" ++ s ++ "]"

curry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
curry3 f (a, b, c) = f a b c


-- test :: Double -> [[Double]] -> String
-- test err input = jsifyVector jsifyBezier $ V.fromList $ fitCurve (V.toList $ readPoints input) err Closed
