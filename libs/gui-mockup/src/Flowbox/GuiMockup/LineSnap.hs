module Flowbox.GuiMockup.LineSnap where

import Math.Coordinate.Cartesian
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
    in {--trace ("\nstart is: "++show start++"\nbezierLength: "++show bezierLength++"\nrest: "++show rest) $ --} deCasteljauCubic rest intersectingBezier 

assignControlPoints :: [CubicBezier Float] -> [CubicBezier Float] -> [(Point2 Float, Point2 Float, Point2 Float)]
assignControlPoints original strokeAproximation = 
    let originalDistancesNorm = normalizeDistances $ bezierDistances original
        strokeDistancesNorm = normalizeDistances $ bezierDistances strokeAproximation
        strokeWithDists = zip (tail strokeDistancesNorm) strokeAproximation
    in {--trace ("\nstrokeDistancesNorm: "++show strokeDistancesNorm) $--} map (flip assignControlPoint strokeWithDists) $ tail originalDistancesNorm

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
