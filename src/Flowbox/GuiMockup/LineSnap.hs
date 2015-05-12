module Flowbox.GuiMockup.LineSnap 
    ( module Flowbox.GuiMockup.LineSnap
    , module Math.Coordinate.Cartesian
        ) where

import Math.Coordinate.Cartesian
--import Linear
import Debug.Trace
--import qualified Prelude as P

data CubicBezier a = CubicBezier { cubicC0 :: Point2 a
                                 , cubicC1 :: Point2 a
                                 , cubicC2 :: Point2 a
                                 , cubicC3 :: Point2 a
                                 } deriving (Eq, Ord, Show)

instance Functor CubicBezier where
    fmap f (CubicBezier a b c d) = CubicBezier (fmap f a) (fmap f b) (fmap f c) (fmap f d)

--instance Applicative CubicBezier where
--    pure a = CubicBezier (pure a) (pure a) (pure a) (pure a)
--    {-# INLINE pure #-}
--    CubicBezier a b c d <*> CubicBezier e f g h = CubicBezier (a <*> e) (b <*> f) (c <*> g) (d <*> h)
--    {-# INLINE (<*>) #-}

-- for testing
streight1 :: CubicBezier Float
streight1 = CubicBezier (Point2 0 0) (Point2 0.2 0) (Point2 0.8 0) (Point2 1 0)

streight2 :: CubicBezier Float
streight2 = CubicBezier (Point2 0 0) (Point2 0.4 0) (Point2 1.6 0) (Point2 2 0)

exampleCurve :: CubicBezier Float
exampleCurve = CubicBezier (Point2 0 0) (Point2 4 0) (Point2 5 1) (Point2 5 5)

exampleCurve2 :: CubicBezier Float
exampleCurve2 = CubicBezier (Point2 5 5) (Point2 5 10) (Point2 15 25) (Point2 25 25)

exampleCurve3 :: CubicBezier Float
exampleCurve3 = CubicBezier (Point2 25 25) (Point2 30 25) (Point2 30 40) (Point2 50 50)

examplePoints :: [Point2 Float]
examplePoints = [Point2 10 0, Point2 11 0, Point2 12 1, Point2 12 5, Point2 12 6]

examplePoints2 :: [Point2 Float]
examplePoints2 = [Point2 1 0, Point2 5 0, Point2 5 5, Point2 6 5]

-- real stuff
arcLength :: CubicBezier Float -> Float
arcLength curve = 
    let points = map (bezierPoint curve) [0.1, 0.2..1] -- fixed 10 segments division
    in fst $ foldl lengthCounter (0, cubicC0 curve) points  

lengthCounter :: (Float, Point2 Float) -> Point2 Float -> (Float, Point2 Float)
lengthCounter (acc, pt1) pt2 = (acc + euclidianDistance pt1 pt2, pt2)

bezierPoint :: CubicBezier Float -> Float -> Point2 Float
bezierPoint curve t = 
    let Point2 x0 y0 = cubicC0 curve
        Point2 x1 y1 = cubicC1 curve
        Point2 x2 y2 = cubicC2 curve
        Point2 x3 y3 = cubicC3 curve
        formula start control1 control2 end = 
            start*(1-t)*(1-t)*(1-t) + 3.0*control1*(1.0-t)*(1.0-t)*t + 3.0*control2*(1.0-t)*t*t + end*t*t*t
    in Point2 (formula x0 x1 x2 x3) (formula y0 y1 y2 y3)

euclidianDistance :: Point2 Float -> Point2 Float -> Float
euclidianDistance (Point2 x1 y1) (Point2 x2 y2)= sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

strokeDistances points = map fst $ scanl lengthCounter (0, head points) (tail points)

bezierDistances beziers = scanl (\acc c -> acc+arcLength c) 0 beziers
--bezierDistances beziers = map arcLength beziers

normalizeDistances distances = map (/(last distances)) distances

--assignPoint' :: Float -> [(Float, Point2 Float)] -> ([(Float, Point2 Float)], Point2 Float)
--assignPoint' bezierLength strokeWithDists =
--    let (start, end) = span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
--        startP = snd $ last start
--        endP = {--trace ("end is: "++show end) $--} snd $ head end
--        rest = ( bezierLength - (fst (last start)) )* (last (strokeDistances (map snd strokeWithDists))) --bad bad bad
--        Point2 u v = endP - startP
--        normU = u / euclidianDistance endP startP
--        normV = v / euclidianDistance endP startP
--        vect = (Point2 rest rest) * (Point2 normU normV) :: Point2 Float
--        resultP = startP + vect
--        endDistance = bezierLength --fst $ head end

--    in (map (\(x,y)->(x-endDistance,y)) ((endDistance,resultP):end), resultP)

-- old 
assignPoint :: Float -> [(Float, Point2 Float)] -> Point2 Float
assignPoint bezierLength strokeWithDists =
    let (start, end) = span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
        startP = snd $ last start
        endP =snd $ head end
        rest = ( bezierLength - (fst (last start)) ) * (last (strokeDistances (map snd strokeWithDists)))
        Point2 u v = endP - startP
        normU = u / euclidianDistance endP startP
        normV = v / euclidianDistance endP startP
        vect = (Point2 rest rest) * (Point2 normU normV) :: Point2 Float
        resultP = startP + vect
    in {-- trace ("\nbezierLength: "++show bezierLength++"\nstartP: "++show startP++"\nendP: "++show endP++"\nrest: "++show rest++"\nnorm u: "++show normU++"\nnorm v: "++show normV++"\nvect: "++show vect) $--}
        resultP

assignControlPoint :: Float -> [(Float, CubicBezier Float)] -> (Point2 Float, Point2 Float, Point2 Float)
assignControlPoint bezierLength strokeWithDists = 
    let (start, end) = span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
        intersectingBezier = snd $ head end
        startDistance = if null start then 0.0 else fst (last start)
        rest = (bezierLength - startDistance)/( (fst (head end)) - startDistance) -- * (sum $ map (\(_,x) -> arcLength x) strokeWithDists)
    in deCasteljauCubic rest intersectingBezier 

assignControlPoint' :: Float -> [(Float, CubicBezier Float)] -> (Point2 Float, Point2 Float, Point2 Float)
assignControlPoint' bezierLength strokeWithDists = 
    let (start, end) = span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
        intersectingBezier = snd $ head end
        startDistance = if null start then 0.0 else fst (last start)
        rest = (bezierLength - startDistance)/( (fst (head end)) - startDistance) -- * (sum $ map (\(_,x) -> arcLength x) strokeWithDists)
        (p, hi, ho) = deCasteljauCubic rest intersectingBezier 
        restPoint = Point2 (1/rest) (1/rest)
        invertRestPoint = Point2 (1/(1-rest)) (1/(1-rest))
    in (p, p+restPoint*(hi-p),  p+invertRestPoint*(ho-p) ) --{--trace ("\nstart is: "++show start++"\nbezierLength: "++show bezierLength++"\nrest: "++show rest) $ --} deCasteljauCubic rest intersectingBezier 

placePoint :: Float -> [(Float, CubicBezier Float)] -> (Point2 Float, Point2 Float, Point2 Float)
placePoint bezierLength strokeWithDists = 
    let (start, end) = span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
        intersectingBezier = snd $ head end
        startDistance = if null start then 0.0 else fst (last start)
        endDistance = if null end then 1.0 else fst (head end)
        rest = (bezierLength - startDistance)/( (fst (head end)) - startDistance) -- * (sum $ map (\(_,x) -> arcLength x) strokeWithDists)
    in deCasteljauCubic rest intersectingBezier 

normVec p = 
    let vecLen = euclidianDistance (Point2 0 0) p
        lenPoint = Point2 vecLen vecLen
    in p/lenPoint

assignControlPoint'' :: (Float, Float, Float) -> [(Float, CubicBezier Float)] -> (Point2 Float, Point2 Float, Point2 Float)
assignControlPoint'' (prev, bezierLength, next) strokeWithDists = 
    let (start, end) = span (\(dist,_) -> {-- trace ("dist is: "++show dist) $--} dist<bezierLength) strokeWithDists
        intersectingBezier = snd $ head end
        startDistance = if null start then 0.0 else fst (last start)
        endDistance = if null end then 1.0 else fst (head end)
        rest = (bezierLength - startDistance)/( (fst (head end)) - startDistance) -- * (sum $ map (\(_,x) -> arcLength x) strokeWithDists)
        (p, hi, ho) = deCasteljauCubic rest intersectingBezier 
        (pPrev,_,_) = placePoint prev strokeWithDists
        (pNext,_,_) = placePoint next strokeWithDists
        --restPoint = Point2 (3/rest) (3/rest)
        --invertRestPoint = Point2 (3/(1-rest)) (3/(1-rest))
        Point2 x1 y1 = (normVec (pPrev-p))*(normVec (hi-p))
        Point2 x2 y2 = (normVec (pNext-p))*(normVec (ho-p))
        --dotProduct = 
        cosPHi = 1 --max 0.75 (x1 + y1)
        cosPHo = 1 --max 0.75 (x2 + y2)
        ratio1 = (1/cosPHi)*(bezierLength - prev)/(bezierLength - startDistance)
        ratio2 = (1/cosPHo)*(next - bezierLength )/(endDistance - bezierLength)
        ratioPoint1 = Point2 ratio1 ratio1
        ratioPoint2 = Point2 ratio2 ratio2
    in (p, p+ratioPoint1*(hi-p),  p+ratioPoint2*(ho-p) ) --{--trace ("\nstart is: "++show start++"\nbezierLength: "++show bezierLength++"\nrest: "++show rest) $ --} deCasteljauCubic rest intersectingBezier 

-- main function of this module
assignControlPoints :: [CubicBezier Float] -> [CubicBezier Float] -> [(Point2 Float, Point2 Float, Point2 Float)]
assignControlPoints original strokeAproximation = 
    let originalDistancesNorm = normalizeDistances $ bezierDistances original
        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
        strokeWithDists = zip (tail strokeDistancesNorm) strokeAproximation
    in {--trace ("\nstrokeDistancesNorm: "++show strokeDistancesNorm) $--} map (flip assignControlPoint strokeWithDists) $ tail originalDistancesNorm

assignControlPoints' :: [CubicBezier Float] -> [CubicBezier Float] -> [(Point2 Float, Point2 Float, Point2 Float)]
assignControlPoints' original strokeAproximation = 
    let originalDistancesNorm = normalizeDistances $ bezierDistances original
        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
        strokeWithDists = zip (tail strokeDistancesNorm) strokeAproximation
    in {--trace ("\nstrokeDistancesNorm: "++show strokeDistancesNorm) $--} map (flip assignControlPoint' strokeWithDists) $ tail originalDistancesNorm

assignControlPoints'' :: [CubicBezier Float] -> [CubicBezier Float] -> [(Point2 Float, Point2 Float, Point2 Float)]
assignControlPoints'' original strokeAproximation = 
    let originalDistancesNorm = normalizeDistances $ bezierDistances original
        originalDistancesNormDoubleEnd = originalDistancesNorm ++ [last originalDistancesNorm]
        originalDistancesNormWithNeighbours = zip3 originalDistancesNormDoubleEnd (tail originalDistancesNormDoubleEnd) (tail (tail originalDistancesNormDoubleEnd))
        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
        strokeWithDists = zip (tail strokeDistancesNorm) strokeAproximation
    in {--trace ("\nstrokeDistancesNorm: "++show strokeDistancesNorm) $--} map (flip assignControlPoint'' strokeWithDists) $ originalDistancesNormWithNeighbours


assignPoints :: [CubicBezier Float] -> [Point2 Float] -> [Point2 Float]
assignPoints beziers stroke = 
    let distBezier = bezierDistances beziers
        normDistBezier = normalizeDistances distBezier --map  (/ sum distBezier) distBezier
        normDistStorke = normalizeDistances $ strokeDistances stroke
        strokeWithDists = zip normDistStorke stroke

    --in map snd $ scanl func ( strokeWithDists , Point2 0 0) (tail normDistBezier) --what to do with first point
    in {--trace ("stWdist: "++show strokeWithDists) $--}
        head stroke : (map (flip assignPoint strokeWithDists) (tail normDistBezier))
    --in (strokeWithDists, normDistBezier)

func = (\(strokeLeft, point) dist -> assignPoint dist strokeLeft)

--func2 = Point2 12 13 + Point2 1 12

controlPointsToStroke :: [CubicBezier Float] -> [(Int,Int)] -> [CubicBezier Float]
controlPointsToStroke controlPoints stroke = controlPoints

deCasteljauCubic :: Float -> CubicBezier Float -> (Point2 Float, Point2 Float, Point2 Float)
deCasteljauCubic t bezier = (point, handle1, handle2) where
    [point, handle1, handle2] = deCasteljau t [cubicC0 bezier, cubicC1 bezier, cubicC2 bezier, cubicC3 bezier]

deCasteljau :: Float -> [Point2 Float] -> [Point2 Float]
deCasteljau t coefs = --trace ("reduced: "++show reduced) $ 
    case coefs of
        [c1,c2] -> reduced ++ coefs 
        _       -> deCasteljau t reduced
    where
        reduced = zipWith (lerpP t) coefs (tail coefs)
        lerpP t (Point2 x0 y0) (Point2 x1 y1) = Point2 (lerp t x0 x1) (lerp t y0 y1)
        lerp t a b = t * b + (1 - t) * a


-- for testing with JFiddle
getPoints :: [String] -> [Point2 Float]
getPoints strings = map (Prelude.uncurry Point2) (getJsPoints strings) where
    getJsPoints :: [String] -> [(Float,Float)]
    getJsPoints = map read

printJsPoints :: [Point2 Float] -> String
printJsPoints points = show $ map (\(Point2 x y) -> [floor x, floor y]) points

process = printJsPoints . (assignPoints [exampleCurve, exampleCurve, exampleCurve, exampleCurve, exampleCurve2, exampleCurve, exampleCurve]) . getPoints 


type PaperPoint = (Float,Float)

type PaperControlPoint = (PaperPoint,PaperPoint,PaperPoint)

s111221 :: [CubicBezier Float]
s111221 = [streight1,streight1,streight1,streight2,streight2,streight1]
--paperPointsToBeziers :: [PaperControlPoint] -> [CubicBezier Float]
paperPointsToBeziers points = 
    tail $ tail $ map snd $ scanl funcR ((0,0), streight1) points

funcR = (\((x,y),bezier) (p,hi,ho) -> ((p,p+ho), CubicBezier x y (p+hi) p))

process2 original = printJsPoints . concat . (map (\(x,y,z) -> [x,y-x,z-x])) . assignControlPoints original . paperPointsToBeziers

process2' original = printJsPoints . concat . (map (\(x,y,z) -> [x,y-x,z-x])) . assignControlPoints' original . paperPointsToBeziers

process2'' original = printJsPoints . concat . (map (\(x,y,z) -> [x,y-x,z-x])) . assignControlPoints'' original . paperPointsToBeziers