{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.GuiMockup.LineSnap 
    ( module Flowbox.GuiMockup.LineSnap
    , module Linear
        ) where

--import Math.Coordinate.Cartesian
import           Linear
import qualified Debug.Trace as D
--import qualified Prelude as P
import qualified Data.Vector.Storable as V
import           Flowbox.GuiMockup.LineFit
import           Foreign.Storable
import           Control.Applicative 
import           Foreign.Ptr
import           Control.Monad        (forM_, when)
import           Control.Monad.ST       (runST) 
import           Data.STRef  
import           Control.Monad.Trans.Class    (lift)
import           Control.Error                hiding (err)




--data CubicBezier a = CubicBezier { cubicC0 :: V2 a
--                                 , cubicC1 :: V2 a
--                                 , cubicC2 :: V2 a
--                                 , cubicC3 :: V2 a
--                                 } deriving (Eq, Ord, Show)

--instance Functor CubicBezier where
--    fmap f (CubicBezier a b c d) = CubicBezier (fmap f a) (fmap f b) (fmap f c) (fmap f d)

--instance Storable a => Storable (CubicBezier a) where
--    sizeOf _ = 4 * sizeOf (undefined :: V2 a)
--    alignment _ = alignment (undefined :: V2 a)
--    peek ptr = CubicBezier <$> peek ptr'
--                           <*> peekElemOff ptr' 1
--                           <*> peekElemOff ptr' 2
--                           <*> peekElemOff ptr' 3
--        where
--            ptr' = castPtr ptr
--    poke ptr (CubicBezier c0 c1 c2 c3) = do
--        let ptr' = castPtr ptr
--        poke ptr' c0
--        pokeElemOff ptr' 1 c1
--        pokeElemOff ptr' 2 c2
--        pokeElemOff ptr' 3 c3

--instance Applicative CubicBezier where
--    pure a = CubicBezier (pure a) (pure a) (pure a) (pure a)
--    {-# INLINE pure #-}
--    CubicBezier a b c d <*> CubicBezier e f g h = CubicBezier (a <*> e) (b <*> f) (c <*> g) (d <*> h)
--    {-# INLINE (<*>) #-}

-- for testing
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

-- real stuff
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

strokeDistances points = V.map fst $ V.scanl lengthCounter (0, V.head points) (V.tail points)

bezierDistances beziers = V.scanl (\acc c -> acc+arcLength c) 0 beziers
--bezierDistances beziers = map arcLength beziers

normalizeDistances distances = V.map (/(V.last distances)) distances

vzip x y = V.zipWith (,) x y

vzip3 x y z = V.zipWith3 (,,) x y z
--assignPoint' :: Float -> [(Float, V2 Float)] -> ([(Float, V2 Float)], V2 Float)
--assignPoint' bezierLength strokeWithDists =
--    let (start, end) = span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
--        startP = snd $ last start
--        endP = {--trace ("end is: "++show end) $--} snd $ head end
--        rest = ( bezierLength - (fst (last start)) )* (last (strokeDistances (map snd strokeWithDists))) --bad bad bad
--        V2 u v = endP - startP
--        normU = u / euclidianDistance endP startP
--        normV = v / euclidianDistance endP startP
--        vect = (V2 rest rest) * (V2 normU normV) :: V2 Float
--        resultP = startP + vect
--        endDistance = bezierLength --fst $ head end

--    in (map (\(x,y)->(x-endDistance,y)) ((endDistance,resultP):end), resultP)

-- old 
assignPoint :: Float -> V.Vector (Float, V2 Float) -> V2 Float
assignPoint bezierLength strokeWithDists =
    let (start, end) = V.span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
        startP = snd $ V.last start
        endP =snd $ V.head end
        rest = ( bezierLength - (fst (V.last start)) ) * (V.last (strokeDistances (V.map snd strokeWithDists)))
        V2 u v = endP - startP
        normU = u / euclidianDistance endP startP
        normV = v / euclidianDistance endP startP
        vect = (V2 rest rest) * (V2 normU normV) :: V2 Float
        resultP = startP + vect
    in {-- trace ("\nbezierLength: "++show bezierLength++"\nstartP: "++show startP++"\nendP: "++show endP++"\nrest: "++show rest++"\nnorm u: "++show normU++"\nnorm v: "++show normV++"\nvect: "++show vect) $--}
        resultP

assignControlPoint :: Float -> V.Vector (Float, CubicBezier Float) -> (V2 Float, V2 Float, V2 Float)
assignControlPoint bezierLength strokeWithDists = 
    let (start, end) = V.span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
        intersectingBezier = snd $ V.head end
        startDistance = if V.null start then 0.0 else fst (V.last start)
        rest = (bezierLength - startDistance)/( (fst (V.head end)) - startDistance) -- * (sum $ map (\(_,x) -> arcLength x) strokeWithDists)
    in deCasteljauCubic rest intersectingBezier 

assignControlPoint' :: Float -> V.Vector (Float, CubicBezier Float) -> (V2 Float, V2 Float, V2 Float)
assignControlPoint' bezierLength strokeWithDists = 
    let (start, end) = V.span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
        intersectingBezier = snd $ V.head end
        startDistance = if V.null start then 0.0 else fst (V.last start)
        rest = (bezierLength - startDistance)/( (fst (V.head end)) - startDistance) -- * (sum $ map (\(_,x) -> arcLength x) strokeWithDists)
        (p, hi, ho) = deCasteljauCubic rest intersectingBezier 
        restPoint = V2 (1/rest) (1/rest)
        invertRestPoint = V2 (1/(1-rest)) (1/(1-rest))
    in (p, p+restPoint*(hi-p),  p+invertRestPoint*(ho-p) ) --{--trace ("\nstart is: "++show start++"\nbezierLength: "++show bezierLength++"\nrest: "++show rest) $ --} deCasteljauCubic rest intersectingBezier 

placePoint :: Float -> V.Vector (Float, CubicBezier Float) -> (V2 Float, V2 Float, V2 Float)
placePoint bezierLength strokeWithDists = 
    let (start, end) = V.span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
        intersectingBezier = snd $ V.head end
        startDistance = if V.null start then 0.0 else fst (V.last start)
        endDistance = if V.null end then 1.0 else fst (V.head end)
        rest = (bezierLength - startDistance)/( (fst (V.head end)) - startDistance) -- * (sum $ map (\(_,x) -> arcLength x) strokeWithDists)
    in deCasteljauCubic rest intersectingBezier 

normVec p = 
    let vecLen = euclidianDistance (V2 0 0) p
        lenPoint = V2 vecLen vecLen
    in p/lenPoint

generateFittingBezier :: (Float, Float) -> V.Vector (Float, CubicBezier Float) -> CubicBezier Float
generateFittingBezier (startLength, endLength) strokeWithDists =

    let (beforeStartStrokeBeziers, afterStartStrokeBeziers) = V.span (\(dist,_) -> dist<startLength) strokeWithDists
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

        resampledPoints = if startPointBezier == endPointBezier
                            then resampleBezierFragment startRest endRest startPointBezier 
                            else (resampleBezierFragment startRest 1.0 startPointBezier) V.++ (V.concat (map resampleBezier (V.toList nextBeziers))) V.++ (resampleBezierFragment 0.0 endRest endPointBezier)
        u =  chordLengthParameterize resampledPoints
        tHat1 = ho-startPoint
        tHat2 = hi-endPoint
        generatedBezier = generateBezier resampledPoints u tHat1 tHat2

        --iteration :: Float
        --iteration = runST $ do
        --    u' <- newSTRef u
        --    --splitPoint' <- lift $ newSTRef initialSplitPoint
        --    bezCurve' <- newSTRef generatedBezier

        --    forM_ [(1::Int)..4] $ \_ -> do
        --        uVal <- readSTRef u'
        --        bezCurveVal <- readSTRef bezCurve'
        --        let uPrime = reparameterize resampledPoints uVal bezCurveVal

        --        writeSTRef bezCurve' $ generateBezier resampledPoints uPrime tHat1 tHat2

        --        bezCurveVal <- readSTRef bezCurve'
        --        --let (maxErrorVal, splitPointVal) = computeMaxError points bezCurveVal uPrime

        --        --writeSTRef splitPoint' splitPointVal
        --        --when (maxErrorVal < iterationError) $ left bezCurveVal

        --        writeSTRef u' uPrime

        --    return $ readSTRef bezCurve'
        iteration = runST $ runEitherT $ do
                u' <- lift $ newSTRef u
                bezCurve' <- lift $ newSTRef generatedBezier

                forM_ [(1::Int)..4] $ \_ -> do
                    uVal <- lift $ readSTRef u'
                    bezCurveVal <- lift $ readSTRef bezCurve'
                    let uPrime = reparameterize resampledPoints uVal bezCurveVal

                    lift $ writeSTRef bezCurve' $ generateBezier resampledPoints uPrime tHat1 tHat2

                    bezCurveVal <- lift $ readSTRef bezCurve'

                    lift $ writeSTRef u' uPrime

                lift (readSTRef bezCurve') >>= right
        Right resultBezier = iteration
          
    in resultBezier --CubicBezier startPoint ho hi endPoint

assignControlPoint'' :: (Float, Float, Float) -> V.Vector (Float, CubicBezier Float) -> (V2 Float, V2 Float, V2 Float)
assignControlPoint'' (prev, bezierLength, next) strokeWithDists = 
    let (start, end) = V.span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
        (nextBeziersT,endNext) = V.span (\(dist,_) -> dist<next) end
        (prevBeziersT,endPrev) = V.span (\(dist,_) -> dist<prev) start

        intersectingBezier = {--D.trace ("\nnextBeziers: "++(show nextBeziers)) $--} snd $ V.head end
        startDistance = if V.null start then 0.0 else fst (V.last start)
        endDistance = if V.null end then 1.0 else fst (V.head end)
        rest = (bezierLength - startDistance)/( (fst (V.head end)) - startDistance) -- * (sum $ map (\(_,x) -> arcLength x) strokeWithDists)

        nextBeziers = V.map snd $ V.tail nextBeziersT
        nextIntersectingBezier = snd $ V.head endNext
        nextStartDistance = if V.null nextBeziersT then 0.0 else fst (V.last nextBeziersT)
        nextEndDistance = if V.null endNext then 1.0 else fst (V.head endNext)
        nextRest = (next - nextStartDistance)/( (fst (V.head endNext)) - nextStartDistance)

        prevBeziers = V.map snd $ V.tail prevBeziersT
        prevIntersectingBezier = snd $ V.head endPrev
        prevStartDistance = if V.null prevBeziersT then 0.0 else fst (V.last prevBeziersT)
        prevEndDistance = if V.null endPrev then 1.0 else fst (V.head endPrev)
        prevRest = (prev - prevStartDistance)/( (fst (V.head endPrev)) - prevStartDistance)

        (p, hi, ho) = deCasteljauCubic rest intersectingBezier
        (pPrev,_,hoPrev) = placePoint prev strokeWithDists
        (pNext,hiNext,_) = placePoint next strokeWithDists

        --resample
        resampledPointsPrev = if intersectingBezier == prevIntersectingBezier then resampleBezierFragment prevRest rest intersectingBezier else (resampleBezierFragment prevRest 1.0 prevIntersectingBezier) V.++ (V.concat (map resampleBezier (V.toList prevBeziers))) V.++ (resampleBezierFragment 0.0 rest intersectingBezier)
        uP =  chordLengthParameterize resampledPoints
        (CubicBezier _ _ phi _) = generateBezier resampledPointsPrev uP (normVec (hoPrev-p)) (normVec (hi-p))

        resampledPoints = D.trace ("\nrest: "++show rest++"\nnextRest: "++show nextRest) $ if intersectingBezier == nextIntersectingBezier then resampleBezierFragment rest nextRest intersectingBezier else (resampleBezierFragment rest 1.0 intersectingBezier) V.++ (V.concat (map resampleBezier (V.toList nextBeziers))) V.++ (resampleBezierFragment 0.0 nextRest nextIntersectingBezier)
        u =  chordLengthParameterize resampledPoints


        generatedBezier = generateBezier resampledPoints u (normVec (ho-p)) (normVec (hiNext-p))
        
        uR = reparameterize resampledPoints u generatedBezier
        generatedBezierR = generateBezier resampledPoints uR (normVec (ho-p)) (normVec (hiNext-p))
        uRR = reparameterize resampledPoints uR generatedBezier
        generatedBezierRR = generateBezier resampledPoints uRR (normVec (ho-p)) (normVec (hiNext-p))
        uRRR = reparameterize resampledPoints uRR generatedBezier
        --generatedBezierRRR = generateBezier resampledPoints uRRR (normVec (ho-p)) (normVec (hiNext-p))
        --uRRRR = reparameterize resampledPoints uRRR generatedBezier
        (CubicBezier pp pho _ _) = generateBezier resampledPoints uRRR (normVec (ho-p)) (normVec (hiNext-p))
        --restPoint = V2 (3/rest) (3/rest)
        --invertRestPoint = V2 (3/(1-rest)) (3/(1-rest))
        V2 x1 y1 = (normVec (pPrev-p))*(normVec (hi-p))
        V2 x2 y2 = (normVec (pNext-p))*(normVec (ho-p))
        --dotProduct = 
        cosPHi = D.trace ("\nresampledPoints length: "++show (V.length resampledPoints)) $ 1 --max 0.75 (x1 + y1)
        cosPHo = 1 --max 0.75 (x2 + y2)
        ratio1 = (1/cosPHi)*(bezierLength - prev)/(bezierLength - startDistance)
        ratio2 = (1/cosPHo)*(next - bezierLength )/(endDistance - bezierLength)
        ratioPoint1 = V2 ratio1 ratio1
        ratioV2 = D.trace ("\np: "++show p++"\npp: "++show pp++"pho: "++show pho) $ V2 ratio2 ratio2
        --a =  1--
    in (p, {--phi--} p+ratioPoint1*(hi-p), {--pho--} p+ratioV2*(ho-p) ) --{--trace ("\nstart is: "++show start++"\nbezierLength: "++show bezierLength++"\nrest: "++show rest) $ --} deCasteljauCubic rest intersectingBezier 

-- main function of this module
--assignControlPoints :: V.Vector (CubicBezier Float) -> V.Vector (CubicBezier Float) -> V.Vector (V2 Float, V2 Float, V2 Float)
--assignControlPoints original strokeAproximation = 
--    let originalDistancesNorm = normalizeDistances $ bezierDistances original
--        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
--        strokeWithDists = vzip (V.tail strokeDistancesNorm) strokeAproximation
--    in {--trace ("\nstrokeDistancesNorm: "++show strokeDistancesNorm) $--} V.map (flip assignControlPoint strokeWithDists) $ V.tail originalDistancesNorm

--assignControlPoints' :: V.Vector (CubicBezier Float) -> V.Vector (CubicBezier Float) -> V.Vector (V2 Float, V2 Float, V2 Float)
--assignControlPoints' original strokeAproximation = 
--    let originalDistancesNorm = normalizeDistances $ bezierDistances original
--        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
--        strokeWithDists = vzip (V.tail strokeDistancesNorm) strokeAproximation
--    in {--trace ("\nstrokeDistancesNorm: "++show strokeDistancesNorm) $--} V.map (flip assignControlPoint' strokeWithDists) $ V.tail originalDistancesNorm

assignControlPoints'' :: V.Vector (CubicBezier Float) -> V.Vector (CubicBezier Float) -> V.Vector (V2 Float, V2 Float, V2 Float)
assignControlPoints'' original strokeAproximation = 
    let originalDistancesNorm = normalizeDistances $ bezierDistances original
        originalDistancesNormDoubleEnd = V.snoc originalDistancesNorm (V.last originalDistancesNorm)
        originalDistancesNormWithNeighbours = vzip3 originalDistancesNormDoubleEnd (V.tail originalDistancesNormDoubleEnd) (V.tail (V.tail originalDistancesNormDoubleEnd))
        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
        strokeWithDists = vzip (V.tail strokeDistancesNorm) strokeAproximation
    in {--trace ("\nstrokeDistancesNorm: "++show strokeDistancesNorm) $--} V.map (flip assignControlPoint'' strokeWithDists) $ originalDistancesNormWithNeighbours

optimizeBeziers :: V.Vector (CubicBezier Float) -> V.Vector (CubicBezier Float) -> V.Vector (CubicBezier Float) -- V.Vector (V2 Float, V2 Float, V2 Float)
optimizeBeziers original strokeAproximation = 
    let originalDistancesNorm = normalizeDistances $ bezierDistances original
        --originalDistancesNormDoubleEnd = V.snoc originalDistancesNorm (V.last originalDistancesNorm)
        originalDistancesNormWithNeighbours = vzip originalDistancesNorm (V.tail originalDistancesNorm)
        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
        strokeWithDists = vzip (V.tail strokeDistancesNorm) strokeAproximation
    in V.map (flip generateFittingBezier strokeWithDists) $ originalDistancesNormWithNeighbours

beziersToControlPoints :: V.Vector (CubicBezier Float) -> V.Vector (V2 Float, V2 Float, V2 Float)
beziersToControlPoints beziers = V.snoc (V.map snd $ V.tail $ (V.scanl (\(prevhi,_) (CubicBezier point1 ho hi point2) -> (hi,(point1,prevhi,ho))) (0.0, (firstPoint,dummyHandle,firstPointHo)) beziers)) lastControlPoint where
    CubicBezier firstPoint firstPointHo _ _ = V.head beziers
    dummyHandle = V2 0 0
    CubicBezier _ _ lastPointHi lastPoint = V.last beziers
    lastControlPoint = (lastPoint, lastPointHi, lastPoint) -- for js purpose. Change third element to dummyHandle

beziersToControlPointsTest = beziersToControlPoints $ V.fromList [(CubicBezier (V2 1 1) (V2 2 1) (V2 3 1) (V2 4 1)), (CubicBezier (V2 4 1) (V2 2 2) (V2 3 2) (V2 4 2))]

assignPoints :: V.Vector (CubicBezier Float) -> V.Vector (V2 Float) -> V.Vector (V2 Float)
assignPoints beziers stroke = 
    let distBezier = bezierDistances beziers
        normDistBezier = normalizeDistances distBezier --map  (/ sum distBezier) distBezier
        normDistStorke = normalizeDistances $ strokeDistances stroke
        strokeWithDists = vzip normDistStorke stroke

    --in map snd $ scanl func ( strokeWithDists , V2 0 0) (tail normDistBezier) --what to do with first point
    in {--trace ("stWdist: "++show strokeWithDists) $--}
        V.cons (V.head stroke) (V.map (flip assignPoint strokeWithDists) (V.tail normDistBezier))
    --in (strokeWithDists, normDistBezier)

--func = (\(strokeLeft, point) dist -> assignPoint dist strokeLeft)

--func2 = V2 12 13 + V2 1 12

controlPointsToStroke :: [CubicBezier Float] -> [(Int,Int)] -> [CubicBezier Float]
controlPointsToStroke controlPoints stroke = controlPoints

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


-- for testing with JFiddle
getPoints :: [String] -> [V2 Float]
getPoints strings = map (Prelude.uncurry V2) (getJsPoints strings) where
    getJsPoints :: [String] -> [(Float,Float)]
    getJsPoints = map read

printJsPoints :: [V2 Float] -> String
printJsPoints points = show $ map (\(V2 x y) -> [floor x, floor y]) points

--process = printJsPoints . (assignPoints [exampleCurve, exampleCurve, exampleCurve, exampleCurve, exampleCurve2, exampleCurve, exampleCurve]) . getPoints 


type PaperPoint = (Float,Float)

type PaperControlPoint = (PaperPoint,PaperPoint,PaperPoint)

s111221 :: V.Vector (CubicBezier Float)
s111221 = V.fromList [streight1,streight1,streight1,streight2,streight2,streight1]
--paperPointsToBeziers :: [PaperControlPoint] -> [CubicBezier Float]
paperPointsToBeziers points = 
    V.fromList $ tail $ tail $ map snd $ scanl funcR ((0,0), streight1) points

funcR = (\((x,y),bezier) (p,hi,ho) -> ((p,p+ho), CubicBezier x y (p+hi) p))

--process2 original = printJsPoints . concat . (map (\(x,y,z) -> [x,y-x,z-x])) . assignControlPoints original . paperPointsToBeziers

--process2' original = printJsPoints . concat . (map (\(x,y,z) -> [x,y-x,z-x])) . assignControlPoints' original . paperPointsToBeziers

process2'' original = printJsPoints . concat . (map (\(x,y,z) -> [x,y-x,z-x])) . V.toList . assignControlPoints'' original . paperPointsToBeziers

process original = printJsPoints . concat . (map (\(x,y,z) -> [x,y-x,z-x])) . V.toList . beziersToControlPoints . optimizeBeziers original . paperPointsToBeziers