module Flowbox.GuiMockup.LineSnap
    ( module Flowbox.GuiMockup.LineSnap
    , module Linear
    , Openness(..)
        ) where

import           Control.Error             hiding (err)
import           Control.Lens
import           Control.Lens.Operators
import           Control.Monad             (forM_, when)
import           Control.Monad.ST          (runST)
import qualified Data.List                 as List
import           Data.STRef
import qualified Data.Vector.Storable      as V
import           Flowbox.GuiMockup.LineFit
import           Foreign.Storable
import           Linear

-- import qualified Debug.Trace as D


type PointC = V2 Double
type LeftHandle = V2 Double
type RightHandle = V2 Double
type ControlPoint = (V2 Double, V2 Double, V2 Double) -- (PointC,LeftHandle,RightHandle)

data Direction = ClockWise | CounterClockWise deriving(Show, Eq)

--main function for gui

guiLineSnap :: [ControlPoint] -> Maybe ControlPoint -> Maybe ControlPoint -> [V2 Double] -> Double -> Openness -> [ControlPoint]
guiLineSnap originalCurveControlPoints pointBefore pointAfter strokePoints errorParameter openness = resultControlPoints
    where
        resultControlPoints = case openness of
            Closed -> startControlPoint : midPoints
            Open   -> startControlPoint : midPoints ++ [endControlPoint]
        midPoints = beziersToFullControlPoints resultCurve
        (startControlPoint, endControlPoint) = endsHandling openness resultCurve originalCurveControlPoints
        --resultCurve = moveCurveToStroke (controlPointsToBeziers originalCurveControlPoints) strokePoints errorParameter
        resultCurve = moveCurveToStroke' originalCurve pointBefore pointAfter strokePoints'' errorParameter openness
        originalCurve = controlPointsToBeziers originalCurveControlPoints'

        strokePointsV = V.fromList strokePoints
        curvePoints = V.concatMap resampleBezier (V.fromList originalCurve)
        originalCurveControlPoints' = case openness of
            Closed -> originalCurveControlPoints ++ [head originalCurveControlPoints]
            Open   -> originalCurveControlPoints
        closestPointIdx = findStartPoint strokePointsV curvePoints originalCurveControlPoints
        originalCurveDirection = if (findDirection curvePoints >= 0) then ClockWise else CounterClockWise
        strokeDirection = if (findDirection strokePointsV >= 0) then ClockWise else CounterClockWise

        strokePoints'  = take (length strokePoints) $ drop closestPointIdx $ cycle strokePoints
        strokePoints'' = case openness of
            Closed -> if strokeDirection==originalCurveDirection then strokePoints' else reverse strokePoints' --D.trace ("\n\ncurve box: " ++show curveBox++"\nstroke box: "++show strokeBox++"\n\n" ) $
            Open   -> strokePoints

findStartPoint strokePointsV curvePoints originalCurveControlPoints =
    V.minIndex (V.map (euclidianDistance (V.head boxedControlPoints)) strokePointsV)
    where
        strokeBox@(ls, rs, us, ds) = boxPoints strokePointsV
        curveBox@(lc, rc, uc, dc) = boxPoints curvePoints

        boxedControlPoints = V.fromList $ map box originalCurveControlPoints
        box :: ControlPoint -> V2 Double
        box (V2 x y, hi, ho) = V2 (tox' x) (toy' y)
        tox' :: Double -> Double
        tox' x = ((x-lc)/(rc-lc))*(rs-ls)+ls
        toy' :: Double -> Double
        toy' y = ((y-dc)/(uc-dc))*(us-ds)+ds

spv = V.fromList [0, 1, 2, 3, 4, V2 5 4, V2 6 4,V2 7 4] :: V.Vector (V2 Double)
cp = V.fromList [V2 7 12, V2 9 10, 10, 11, 12] :: V.Vector (V2 Double)
occp = [(V2 7 12,undefined, undefined),(12,undefined, undefined)] :: [ControlPoint]

endsHandling :: Openness -> [CubicBezier Double] -> [ControlPoint] -> (ControlPoint, ControlPoint)
endsHandling openness resultCurve originalCurveControlPoints = (startControlPoint, endControlPoint) where
    startControlPoint = case openness of
        Closed -> (ps, hi, ho)
        Open   -> (ps , his', ho)
    endControlPoint = (pe, hi, hoe')
    his' = his + (ps - originalStartPoint)
    hoe' = hoe + (pe - originalEndPoint)
    (originalStartPoint, his, _) = head originalCurveControlPoints
    (originalEndPoint  , _, hoe) = last originalCurveControlPoints
    CubicBezier ps ho _ _ = head resultCurve
    CubicBezier _ _ hi pe = last resultCurve

findDirection points = fst $ V.foldl (\(direction, V2 x1 y1) pt@(V2 x2 y2) -> (direction+(x2-x1)*(y2+y1), pt)) (0, V.last points) points

boxPoints points = (l, r, u, d)
    where
        l = (V.minimum points) ^._x
        r = (V.maximum points) ^._x
        u = (V.maximumBy (\(V2 x1 y1) (V2 x2 y2) -> compare y1 y2) points) ^._y
        d = (V.minimumBy (\(V2 x1 y1) (V2 x2 y2) -> compare y1 y2) points) ^._y
--moveCurveToStroke :: [CubicBezier Double] -> [V2 Double] -> Double -> Openness -> [CubicBezier Double]
--moveCurveToStroke originalCurve strokePoints errorParameter openness = V.toList $ optimizeBeziers (V.fromList originalCurve) (V.fromList strokeAproximation)
--     where
--         strokeAproximation = fitCurve' strokePoints errorParameter openness

moveCurveToStroke' :: [CubicBezier Double] -> Maybe ControlPoint -> Maybe ControlPoint -> [V2 Double] -> Double -> Openness -> [CubicBezier Double]
moveCurveToStroke' originalCurve pointBefore pointAfter strokePoints errorParameter openness = V.toList $ optimizeBeziers' (V.fromList originalCurve) pointBefore pointAfter (V.fromList strokeAproximation)
    where
        strokeAproximation = controlPointsToBeziers $ fitCurve strokePoints errorParameter openness


optimizeBeziers :: V.Vector (CubicBezier Double) -> V.Vector (CubicBezier Double) -> V.Vector (CubicBezier Double) -- V.Vector (V2 Double, V2 Double, V2 Double)
optimizeBeziers original strokeAproximation =
    let originalDistancesNorm = normalizeDistances $ bezierDistances original
        --originalDistancesNormDoubleEnd = V.snoc originalDistancesNorm (V.last originalDistancesNorm)
        originalDistancesNormWithNeighbours = vzip originalDistancesNorm (V.tail originalDistancesNorm)
        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
        strokeWithDists = vzip (V.tail strokeDistancesNorm) strokeAproximation
    in V.map (flip generateFittingBezier strokeWithDists) originalDistancesNormWithNeighbours

optimizeBeziers' :: V.Vector (CubicBezier Double) -> Maybe ControlPoint -> Maybe ControlPoint -> V.Vector (CubicBezier Double) -> V.Vector (CubicBezier Double) -- V.Vector (V2 Double, V2 Double, V2 Double)
optimizeBeziers' original pointBefore pointAfter strokeAproximation =
    let originalDistancesNorm' = normalizeDistances $ bezierDistances original
        originalDistancesNorm = case (pointBefore, pointAfter) of
                                    (Nothing, Nothing) -> originalDistancesNorm'
                                    (Just _, Nothing)  -> adjustStartDistance originalDistancesNorm'
                                    (Nothing, Just _)  -> adjustEndDistance originalDistancesNorm'
                                    (Just _, Just _)   -> adjustEndDistance $ adjustStartDistance originalDistancesNorm' -- unfortunately order of function matters
        --originalDistancesNormDoubleEnd = V.snoc originalDistancesNorm (V.last originalDistancesNorm)
        originalDistancesNormWithNeighbours = vzip originalDistancesNorm (V.tail originalDistancesNorm)
        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
        strokeWithDists = vzip (V.tail strokeDistancesNorm) strokeAproximation
    in V.map (flip generateFittingBezier strokeWithDists) originalDistancesNormWithNeighbours

adjustStartDistance :: (V.Vector Double) -> (V.Vector Double)
adjustStartDistance distances = normalizeDistances $ V.map (+1/(fromIntegral $ V.length distances)) distances -- because of normalization step this function has to be applied first

adjustEndDistance :: (V.Vector Double) -> (V.Vector Double)
adjustEndDistance distances = V.map (* ((len-1)/len) ) distances
    where
        len = fromIntegral $ V.length distances

-- prototypes
adjustStartDistance' :: (V.Vector Double) -> (V.Vector Double)
adjustStartDistance' distances = normalizeDistances $ V.map (+ V.head distances) distances -- because of normalization step this function has to be applied first

adjustEndDistance' :: (V.Vector Double) -> (V.Vector Double)
adjustEndDistance' distances = V.map (* proportion ) distances
    where
        proportion = 1/(1.0 + V.last distances)

-- end of prototypes

generateFittingBezier :: (Double, Double) -> V.Vector (Double, CubicBezier Double) -> CubicBezier Double
generateFittingBezier (startLength, endLength) strokeWithDists = iteration
    where
        iteration = runST $ do
            u' <- newSTRef u
            bezCurve' <- newSTRef generatedBezier

            forM_ [(1::Int)..20] $ \_ -> do
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

--beziersToControlPoints :: V.Vector (CubicBezier Double) -> V.Vector (V2 Double, V2 Double, V2 Double)
--beziersToControlPoints beziers = --V.snoc (V.map snd $ V.tail $ V.scanl (\(prevhi,_) (CubicBezier point1 ho hi point2) -> (hi,(point1,prevhi,ho))) (0.0, (firstPoint,dummyHandle,firstPointHo)) beziers) lastControlPoint where
    --CubicBezier firstPoint firstPointHo _ _ = V.head beziers
    --dummyHandle = V2 0 0
    --CubicBezier _ _ lastPointHi lastPoint = V.last beziers
    --lastControlPoint = (lastPoint, lastPointHi, lastPoint) -- for js purpose. Change third element to dummyHandle

-- Function returns control points where both handles are well defined (first point of first segment and second point of last segment are ignored)
beziersToFullControlPoints :: [CubicBezier Double] -> [ControlPoint]
beziersToFullControlPoints beziers = map snd $ tail . tail $ scanl (\(prevhi,_) (CubicBezier point1 ho hi point2) -> (hi,(point1,prevhi,ho))) (0.0, (firstPoint,dummyHandle,firstPointHo)) beziers where
    CubicBezier firstPoint firstPointHo _ _ = head beziers
    dummyHandle = V2 0 0
    CubicBezier _ _ lastPointHi lastPoint = last beziers
    --lastControlPoint = (lastPoint, lastPointHi, lastPoint) -- for js purpose. Change third element to dummyHandle

beziersToControlPoints :: [CubicBezier Double] -> [ControlPoint]
beziersToControlPoints beziers = [startControlPoint] ++ midPoints ++ [endControlPoint]
    where
        midPoints = beziersToFullControlPoints beziers
        startControlPoint = (ps , ps, ho)
        endControlPoint = (pe, hi, pe)
        CubicBezier ps ho _ _ = head beziers
        CubicBezier _ _ hi pe = last beziers

controlPointsToBeziers :: [ControlPoint] -> [CubicBezier Double]
controlPointsToBeziers controlPoints =
    tail $ tail $ map snd $ scanl funcRR ((0,0), streight1) controlPoints
        where
            funcRR = (\((x,y),bezier) (p,hi,ho) -> ((p,ho), CubicBezier x y hi p))

deCasteljauCubic :: Double -> CubicBezier Double -> (V2 Double, V2 Double, V2 Double)
deCasteljauCubic t bezier = (point, handle1, handle2) where
    [point, handle1, handle2] = deCasteljau t [cubicC0 bezier, cubicC1 bezier, cubicC2 bezier, cubicC3 bezier]

deCasteljau :: Double -> [V2 Double] -> [V2 Double]
deCasteljau t coefs = --trace ("reduced: "++show reduced) $
    case coefs of
        [c1,c2] -> reduced ++ coefs
        _       -> deCasteljau t reduced
    where
        reduced = zipWith (lerpP t) coefs (tail coefs)
        lerpP t (V2 x0 y0) (V2 x1 y1) = V2 (lerp t x0 x1) (lerp t y0 y1)
        lerp t a b = t * b + (1 - t) * a

bezierFormula formula curve t =
    let V2 x0 y0 = cubicC0 curve
        V2 x1 y1 = cubicC1 curve
        V2 x2 y2 = cubicC2 curve
        V2 x3 y3 = cubicC3 curve
     in V2 (formula x0 x1 x2 x3) (formula y0 y1 y2 y3)

bezierFirstDerivative :: CubicBezier Double -> Double -> V2 Double
bezierFirstDerivative curve t = bezierFormula formula curve t where
    formula start control1 control2 end =
        3.0*(1-t)*(1-t)*(control1-start) + 6.0*(control2-control1)*(1.0-t)*t + 3.0*(end-control2)*t*t
--bezierDerivative curve t =
--    let V2 x0 y0 = cubicC0 curve
--        V2 x1 y1 = cubicC1 curve
--        V2 x2 y2 = cubicC2 curve
--        V2 x3 y3 = cubicC3 curve
--
--
--    in V2 (formula x0 x1 x2 x3) (formula y0 y1 y2 y3)

bezierSecondDerivative :: CubicBezier Double -> Double -> V2 Double
bezierSecondDerivative curve t = bezierFormula formula curve t where
    formula start control1 control2 end =
        6.0*(1-t)*(control2-2*control1+start) + 6.0*(end-2*control2+control1)*t

endPointsCurvature (CubicBezier p0 p1 p2 p3) = (2/3)*(pointLineDistance p2 p1 p0)/((euclidianDistance p0 p1)**2)

pointLineDistance (V2 x y) (V2 x0 y0) (V2 x1 y1) =
    abs $ ((y0-y1)*x+(x1-x0)*y+(x0*y1-x1*y0))/(sqrt ((x1-x0)*(x1-x0)+(y1-y0)*(y1-y0)))

arcLength :: CubicBezier Double -> Double
arcLength curve =
    let points = map (bezierPoint curve) [0.1, 0.2..1] -- fixed 10 segments division
    in fst $ foldl lengthCounter (0, cubicC0 curve) points

lengthCounter :: (Double, V2 Double) -> V2 Double -> (Double, V2 Double)
lengthCounter (acc, pt1) pt2 = (acc + euclidianDistance pt1 pt2, pt2)

bezierPoint :: CubicBezier Double -> Double -> V2 Double
bezierPoint curve t =
    let V2 x0 y0 = cubicC0 curve
        V2 x1 y1 = cubicC1 curve
        V2 x2 y2 = cubicC2 curve
        V2 x3 y3 = cubicC3 curve
        formula start control1 control2 end =
            start*(1-t)*(1-t)*(1-t) + 3.0*control1*(1.0-t)*(1.0-t)*t + 3.0*control2*(1.0-t)*t*t + end*t*t*t
    in V2 (formula x0 x1 x2 x3) (formula y0 y1 y2 y3)

resampleBezier :: CubicBezier Double -> V.Vector (V2 Double)
resampleBezier = resampleBezierFragment 0.0 1.0

resampleBezierFragment :: Double -> Double -> CubicBezier Double -> V.Vector (V2 Double)
resampleBezierFragment start end bezier = if start==end then V.singleton (bezierPoint bezier start) else V.map (bezierPoint bezier) (V.fromList [start,start+((end-start)/10)..end])

euclidianDistance :: V2 Double -> V2 Double -> Double
euclidianDistance (V2 x1 y1) (V2 x2 y2)= sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

--For stroke aproximation. Replaced by LineFit module
--strokeDistances points = V.map fst $ V.scanl lengthCounter (0, V.head points) (V.tail points)

bezierDistances :: V.Vector (CubicBezier Double) -> V.Vector Double
bezierDistances = V.scanl (\acc c -> acc+arcLength c) 0
--bezierDistances beziers = map arcLength beziers

normalizeDistances :: V.Vector Double -> V.Vector Double
normalizeDistances distances = V.map (/ V.last distances) distances

vzip :: (Storable a, Storable b) => V.Vector a -> V.Vector b -> V.Vector (a,b)
vzip = V.zipWith (,)


-- helpers for testing with JFiddle
getPoints :: [String] -> [V2 Double]
getPoints strings = map (Prelude.uncurry V2) (getJsPoints strings) where
    getJsPoints :: [String] -> [(Double,Double)]
    getJsPoints = map read

printJsPoints :: [V2 Double] -> String
printJsPoints points = show $ map (\(V2 x y) -> [floor x, floor y]) points

paperPointsToBeziers points =
    V.fromList $ tail $ tail $ map snd $ scanl funcR ((0,0), streight1) points

funcR = (\((x,y),bezier) (p,hi,ho) -> ((p,p+ho), CubicBezier x y (p+hi) p))

-- test function
--process original = printJsPoints . concatMap (\(x,y,z) -> [x,y-x,z-x]) . V.toList . beziersToControlPoints . optimizeBeziers original . paperPointsToBeziers

-- test data
s111221 :: V.Vector (CubicBezier Double)
s111221 = V.fromList [streight1,streight1,streight1,streight2,streight2,streight1]

streight1 :: CubicBezier Double
streight1 = CubicBezier (V2 0 0) (V2 0.2 0) (V2 0.8 0) (V2 1 0)

streight2 :: CubicBezier Double
streight2 = CubicBezier (V2 0 0) (V2 0.4 0) (V2 1.6 0) (V2 2 0)

exampleCurve :: CubicBezier Double
exampleCurve = CubicBezier (V2 0 0) (V2 4 0) (V2 5 1) (V2 5 5)

exampleCurve2 :: CubicBezier Double
exampleCurve2 = CubicBezier (V2 5 5) (V2 5 10) (V2 15 25) (V2 25 25)

exampleCurve3 :: CubicBezier Double
exampleCurve3 = CubicBezier (V2 25 25) (V2 30 25) (V2 30 40) (V2 50 50)

examplePoints :: V.Vector (V2 Double)
examplePoints = V.fromList [V2 10 0, V2 11 0, V2 12 1, V2 12 5, V2 12 6]

examplePoints2 :: V.Vector (V2 Double)
examplePoints2 = V.fromList [V2 1 0, V2 5 0, V2 5 5, V2 6 5]
