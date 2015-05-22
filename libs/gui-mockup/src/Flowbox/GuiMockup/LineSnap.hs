module Flowbox.GuiMockup.LineSnap 
    ( module Flowbox.GuiMockup.LineSnap
    , module Linear
        ) where

import           Control.Error                hiding (err)
import           Control.Monad                (forM_, when)
import           Control.Monad.ST             (runST)
import qualified Data.Vector.Storable         as V
import           Data.STRef
import           Flowbox.GuiMockup.LineFit  
import           Foreign.Storable
import           Linear



--main function for gui
guiLineSnap :: [CubicBezier Float] -> [V2 Float] -> [CubicBezier Float]
guiLineSnap originalCurve strokePoints = V.toList $ optimizeBeziers (V.fromList originalCurve) (V.fromList strokeAproximation)
    where
        strokeAproximation = fitCurve strokePoints 10

optimizeBeziers :: V.Vector (CubicBezier Float) -> V.Vector (CubicBezier Float) -> V.Vector (CubicBezier Float) -- V.Vector (V2 Float, V2 Float, V2 Float)
optimizeBeziers original strokeAproximation = 
    let originalDistancesNorm = normalizeDistances $ bezierDistances original
        --originalDistancesNormDoubleEnd = V.snoc originalDistancesNorm (V.last originalDistancesNorm)
        originalDistancesNormWithNeighbours = vzip originalDistancesNorm (V.tail originalDistancesNorm)
        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
        strokeWithDists = vzip (V.tail strokeDistancesNorm) strokeAproximation
    in V.map (flip generateFittingBezier strokeWithDists) originalDistancesNormWithNeighbours

generateFittingBezier :: (Float, Float) -> V.Vector (Float, CubicBezier Float) -> CubicBezier Float
generateFittingBezier (startLength, endLength) strokeWithDists = iteration
    where
        iteration = runST $ do
            u' <- newSTRef u
            bezCurve' <- newSTRef generatedBezier

            forM_ [(1::Int)..4] $ \_ -> do
                uVal <- readSTRef u'
                bezCurveVal <- readSTRef bezCurve'
                let uPrime = reparameterize resampledPoints uVal bezCurveVal

                writeSTRef bezCurve' $ generateBezier resampledPoints uPrime tHat1 tHat2

                bezCurveVal <- readSTRef bezCurve'

                writeSTRef u' uPrime

            readSTRef bezCurve'

        generatedBezier = generateBezier resampledPoints u tHat1 tHat2 
        resampledPoints = if startPointBezier == endPointBezier
                            then resampleBezierFragment startRest endRest startPointBezier 
                            else resampleBezierFragment startRest 1.0 startPointBezier V.++ 
                                 V.concat (map resampleBezier (V.toList nextBeziers)) V.++ 
                                 resampleBezierFragment 0.0 endRest endPointBezier
        u = chordLengthParameterize resampledPoints
        tHat1 = ho-startPoint
        tHat2 = hi-endPoint

        (beforeStartStrokeBeziers, afterStartStrokeBeziers) = V.span (\(dist,_) -> dist<startLength) strokeWithDists
        startPointBezier = snd $ V.head afterStartStrokeBeziers
        (beforeEndStrokeBeziers, afterEndStrokeBeziers) = V.span (\(dist,_) -> dist<endLength) afterStartStrokeBeziers
        endPointBezier = snd $ V.head afterEndStrokeBeziers

        beforeStartBeziersDistance = if V.null beforeStartStrokeBeziers then 0.0 else fst (V.last beforeStartStrokeBeziers) --afterStartBeziersDistance = if V.null afterStartStrokeBeziers then 1.0 else fst (V.head afterStartStrokeBeziers) --needed?
        startRest = (startLength - beforeStartBeziersDistance)/( (fst (V.head afterStartStrokeBeziers)) - beforeStartBeziersDistance)

        beforeEndBeziersDistance = if V.null beforeEndStrokeBeziers then beforeStartBeziersDistance else fst (V.last beforeEndStrokeBeziers)
        endRest = (endLength - beforeEndBeziersDistance)/( (fst (V.head afterEndStrokeBeziers)) - beforeEndBeziersDistance)

        (startPoint, _, ho) = deCasteljauCubic startRest startPointBezier
        (endPoint, hi, _) = deCasteljauCubic endRest endPointBezier
        nextBeziers = V.map snd $ V.tail beforeEndStrokeBeziers

beziersToControlPoints :: V.Vector (CubicBezier Float) -> V.Vector (V2 Float, V2 Float, V2 Float)
beziersToControlPoints beziers = V.snoc (V.map snd $ V.tail $ V.scanl (\(prevhi,_) (CubicBezier point1 ho hi point2) -> (hi,(point1,prevhi,ho))) (0.0, (firstPoint,dummyHandle,firstPointHo)) beziers) lastControlPoint where
    CubicBezier firstPoint firstPointHo _ _ = V.head beziers
    dummyHandle = V2 0 0
    CubicBezier _ _ lastPointHi lastPoint = V.last beziers
    lastControlPoint = (lastPoint, lastPointHi, lastPoint) -- for js purpose. Change third element to dummyHandle

deCasteljauCubic :: Float -> CubicBezier Float -> (V2 Float, V2 Float, V2 Float)
deCasteljauCubic t bezier = (point, handle1, handle2) where
    [point, handle1, handle2] = deCasteljau t [cubicC0 bezier, cubicC1 bezier, cubicC2 bezier, cubicC3 bezier]

deCasteljau :: Float -> [V2 Float] -> [V2 Float]
deCasteljau t coefs = --trace ("reduced: "++show reduced) $ 
    case coefs of
        [c1,c2] -> reduced ++ coefs 
        _       -> deCasteljau t reduced
    where
        reduced = zipWith (lerpP t) coefs (tail coefs)
        lerpP t (V2 x0 y0) (V2 x1 y1) = V2 (lerp t x0 x1) (lerp t y0 y1)
        lerp t a b = t * b + (1 - t) * a

arcLength :: CubicBezier Float -> Float
arcLength curve = 
    let points = map (bezierPoint curve) [0.1, 0.2..1] -- fixed 10 segments division
    in fst $ foldl lengthCounter (0, cubicC0 curve) points  

lengthCounter :: (Float, V2 Float) -> V2 Float -> (Float, V2 Float)
lengthCounter (acc, pt1) pt2 = (acc + euclidianDistance pt1 pt2, pt2)

bezierPoint :: CubicBezier Float -> Float -> V2 Float
bezierPoint curve t = 
    let V2 x0 y0 = cubicC0 curve
        V2 x1 y1 = cubicC1 curve
        V2 x2 y2 = cubicC2 curve
        V2 x3 y3 = cubicC3 curve
        formula start control1 control2 end = 
            start*(1-t)*(1-t)*(1-t) + 3.0*control1*(1.0-t)*(1.0-t)*t + 3.0*control2*(1.0-t)*t*t + end*t*t*t
    in V2 (formula x0 x1 x2 x3) (formula y0 y1 y2 y3)

resampleBezier :: CubicBezier Float -> V.Vector (V2 Float)
resampleBezier = resampleBezierFragment 0.0 1.0

resampleBezierFragment :: Float -> Float -> CubicBezier Float -> V.Vector (V2 Float)
resampleBezierFragment start end bezier = if start==end then V.singleton (bezierPoint bezier start) else V.map (bezierPoint bezier) (V.fromList [start,start+((end-start)/10)..end])

euclidianDistance :: V2 Float -> V2 Float -> Float
euclidianDistance (V2 x1 y1) (V2 x2 y2)= sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

--For stroke aproximation. Replaced by LineFit module
--strokeDistances points = V.map fst $ V.scanl lengthCounter (0, V.head points) (V.tail points)

bezierDistances :: V.Vector (CubicBezier Float) -> V.Vector Float
bezierDistances = V.scanl (\acc c -> acc+arcLength c) 0
--bezierDistances beziers = map arcLength beziers

normalizeDistances :: V.Vector Float -> V.Vector Float
normalizeDistances distances = V.map (/ V.last distances) distances

vzip :: (Storable a, Storable b) => V.Vector a -> V.Vector b -> V.Vector (a,b) 
vzip = V.zipWith (,)


-- helpers for testing with JFiddle
getPoints :: [String] -> [V2 Float]
getPoints strings = map (Prelude.uncurry V2) (getJsPoints strings) where
    getJsPoints :: [String] -> [(Float,Float)]
    getJsPoints = map read

printJsPoints :: [V2 Float] -> String
printJsPoints points = show $ map (\(V2 x y) -> [floor x, floor y]) points

paperPointsToBeziers points = 
    V.fromList $ tail $ tail $ map snd $ scanl funcR ((0,0), streight1) points

funcR = (\((x,y),bezier) (p,hi,ho) -> ((p,p+ho), CubicBezier x y (p+hi) p))

-- test function
process original = printJsPoints . concatMap (\(x,y,z) -> [x,y-x,z-x]) . V.toList . beziersToControlPoints . optimizeBeziers original . paperPointsToBeziers

-- test data
s111221 :: V.Vector (CubicBezier Float)
s111221 = V.fromList [streight1,streight1,streight1,streight2,streight2,streight1]

streight1 :: CubicBezier Float
streight1 = CubicBezier (V2 0 0) (V2 0.2 0) (V2 0.8 0) (V2 1 0)

streight2 :: CubicBezier Float
streight2 = CubicBezier (V2 0 0) (V2 0.4 0) (V2 1.6 0) (V2 2 0)

exampleCurve :: CubicBezier Float
exampleCurve = CubicBezier (V2 0 0) (V2 4 0) (V2 5 1) (V2 5 5)

exampleCurve2 :: CubicBezier Float
exampleCurve2 = CubicBezier (V2 5 5) (V2 5 10) (V2 15 25) (V2 25 25)

exampleCurve3 :: CubicBezier Float
exampleCurve3 = CubicBezier (V2 25 25) (V2 30 25) (V2 30 40) (V2 50 50)

examplePoints :: V.Vector (V2 Float)
examplePoints = V.fromList [V2 10 0, V2 11 0, V2 12 1, V2 12 5, V2 12 6]

examplePoints2 :: V.Vector (V2 Float)
examplePoints2 = V.fromList [V2 1 0, V2 5 0, V2 5 5, V2 6 5]
