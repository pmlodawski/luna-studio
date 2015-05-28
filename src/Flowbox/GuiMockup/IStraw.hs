{-# LANGUAGE ViewPatterns #-}

module Flowbox.GuiMockup.IStraw
    ( iStraw
    ) where

import           Control.Applicative          ((<$>), (<*>))
import           Control.Lens.Operators
import           Control.Monad                (forM_, when)
import           Control.Monad.Loops
import           Control.Monad.ST             (ST, runST)
import           Data.Function                (on)
import           Data.STRef
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign.Storable
import           Foreign.Storable.Tuple       ()
import           Linear                       hiding (point, trace)

import           Flowbox.GuiMockup.JSInterop
import           Debug.Trace



determineResampleSpacing :: V.Vector (V2 Float) -> Float
determineResampleSpacing points = diagonal / 40
    where
        diagonal = distance topLeft bottomRight
        topLeft = V2 minX minY
        bottomRight = V2 maxX maxY

        minX = V.minimumBy (compare `on` (^. _x)) points ^. _x
        minY = V.minimumBy (compare `on` (^. _y)) points ^. _y
        maxX = V.maximumBy (compare `on` (^. _x)) points ^. _x
        maxY = V.maximumBy (compare `on` (^. _y)) points ^. _y

-- insertAt :: Int -> a -> [a] -> [a]
-- insertAt n a l = let (ys, zs) = splitAt n l in ys ++ [a] ++ zs

-- removeAt :: Int -> [a] -> [a]
-- removeAt n l = case back of
--     []     -> error "removeAt: index too large"
--     (_:rest) -> front ++ rest
--     where
--         (front, back) = splitAt (n-1) l

remove :: Eq a => a -> [a] -> [a]
remove toDelete l = [ x | x <- l, x /= toDelete]

increment :: Num a => STRef s a -> ST s ()
increment ref = modifySTRef ref (+1)

decrement :: Num a => STRef s a -> ST s ()
decrement ref = modifySTRef ref (subtract 1)

-- pathDistance :: V.Vector (V2 Float) -> Int -> Int -> Float
-- pathDistance points i1 i2 = runST $ do
--     d <- newSTRef 0

--     forM_ [i1..i2] $ \i -> do
--         d' <- readSTRef d
--         writeSTRef d $ d' + distance (points V.! i) (points V.! (i + 1))

--     readSTRef d

resample :: Float -> V.Vector (V2 Float) -> V.Vector (V2 Float)
resample i v@(V.toList -> points) = V.fromList $ go 0 (head points) (tail points) [head points]
    where
        go :: Float -> V2 Float -> [V2 Float] -> [V2 Float] -> [V2 Float]
        go bigD previousPoint (point:ps) newPoints =
            let d = distance previousPoint point
            in  if bigD + d >= i
                then let q = previousPoint + ((i - bigD) / d) *^ (point - previousPoint)
                     in  go 0 q (point:ps) (q:newPoints)
                else go (bigD + d) point ps newPoints
        go _ _ [] newPoints = reverse newPoints


resampleTime :: Float -> V.Vector (V2 Float, Float) -> V.Vector (V2 Float, Float)
resampleTime i v@(V.toList -> points) = V.fromList $ go 0 (head points) (tail points) [head points]
    where
        go :: Float -> (V2 Float, Float) -> [(V2 Float, Float)] -> [(V2 Float, Float)] -> [(V2 Float, Float)]
        go bigD previousPoint (point:ps) newPoints =
            let d = (distance `on` fst) previousPoint point
            in  if bigD + d >= i
                then let q = let factor = ((i - bigD) / d)
                                 (prevVec, prevTime) = previousPoint
                                 (vec, time) = point
                                 interpolate f a b = a + factor `f` (b - a)
                                 newVec = interpolate (*^) prevVec vec
                                 newTime = interpolate (*) prevTime time
                             in  (newVec, newTime)
                     in  go 0 q (point:ps) (q:newPoints)
                else go (bigD + d) point ps newPoints
        go _ _ [] newPoints = reverse newPoints


pathLength :: V.Vector (V2 Float) -> Float
pathLength v = V.foldl' (\d (prev, next) -> d + distance prev next) 0
             $ V.zipWith (,) v (V.tail v)

test3 :: Float -> [[Float]] -> String
test3 n input = jsifyVector jsifyV2 $ resample n $ readPoints input

halfwayCorner :: V.Vector (V2 Float, Float) -> Int
halfwayCorner vec = quarter + V.minIndexBy (compare `on` snd) vec'
    where
        vec'    = V.slice quarter len vec
        len     = V.length vec - 2 * quarter
        quarter = V.length vec `div` 4

postProcessCorners :: V.Vector (V2 Float, Float) -> V.Vector Int -> V.Vector Int
postProcessCorners points (V.toList -> corners) = V.fromList $ go3 (go1 False corners) []
    where
        go1 :: Bool -> [Int] -> [Int]
        go1 (not -> False) acc = acc
        go1 (not -> True)  acc = go2 True acc []
            where
                go2 :: Bool -> [Int] -> [Int] -> [Int]
                go2 currentBool corners@(c1:c2:cs) acc =
                    let slice = V.slice c1 (c2 - c1 + 1) points
                    in  if not $ isLine $ V.map fst slice
                        then let newCorner = c1 + halfwayCorner slice
                             in  if newCorner > c1 && newCorner < c2
                                 then go2 False (newCorner:c2:cs) (c1:acc)
                                 else go2 currentBool (c2:cs) (c1:acc)
                        else go2 currentBool (c2:cs) (c1:acc)
                go2 currentBool [c]                acc = go1 currentBool (reverse $ c:acc)

        go3 :: [Int] -> [Int] -> [Int]
        go3 (c1:c:c2:cs) acc = if isLine $ V.map fst $ V.slice c1 (c2 - c1 + 1) points
                               then go3 (c1:c2:cs) acc
                               else go3 (c:c2:cs) (c1:acc)
        go3 [c1,c2]      acc = reverse $ c2:c1:acc

curveDetection :: V.Vector (V2 Float) -> V.Vector Int -> V.Vector Int
curveDetection points corners = V.fromList $ go (V.toList corners) []
    where
        shift = 15

        go :: [Int] -> [Int] -> [Int]
        go (previousCorner:currentCorner:nextCorner:cs) acc =
            if beta - alpha > threshold
                then go (nextCorner:cs)               (previousCorner:acc)
                else go (currentCorner:nextCorner:cs) (previousCorner:acc)
            where
                preDiff  = currentCorner - previousCorner
                nextDiff = nextCorner - currentCorner

                startIndex = if preDiff  < shift then previousCorner else currentCorner - shift
                endIndex   = if nextDiff < shift then nextCorner     else currentCorner + shift
                alpha = getAngle (points V.! currentCorner) (points V.! startIndex) (points V.! endIndex)

                startIndexBeta = currentCorner - (ceiling $ fromIntegral (currentCorner - startIndex) / 3)
                endIndexBeta   = currentCorner - (ceiling $ fromIntegral (endIndex - currentCorner) / 3)
                beta = getAngle (points V.! currentCorner) (points V.! startIndex) (points V.! endIndex)

                threshold = 10 + 800 / (alpha + 35)

        go [c1,c2] acc = reverse $ c2:c1:acc
        go [c]     acc = reverse $ c:acc

removeAdjacentCorners :: V.Vector (V2 Float) -> V.Vector Float -> V.Vector Int -> V.Vector Int
removeAdjacentCorners points straws corners = V.fromList $ go (V.toList corners) []
    where
        go :: [Int] -> [Int] ->[Int]
        go (prevCorner:currentCorner:cs) acc =
            if currentCorner - prevCorner == 1
                then if straws V.! currentCorner < straws V.! prevCorner
                        then go (currentCorner:cs) acc
                        else go (prevCorner:cs)    acc
                else go (currentCorner:cs) (prevCorner:acc)

getAngle :: V2 Float -> V2 Float -> V2 Float -> Float
getAngle center p1 p2 = angle * 180 / pi
    where
        angle = acos $ v1 ^. _x * v2 ^. _x + v1 ^. _y * v2  ^. _y
        v1 = normalize $ p1 - center
        v2 = normalize $ p2 - center

isLine :: V.Vector (V2 Float) -> Bool
isLine vec = len / pathLen > threshold
    where
        len = distance (V.head vec) (V.last vec)
        pathLen = pathLength vec
        threshold = 0.95

getCorners :: V.Vector (V2 Float) -> V.Vector Int
getCorners points = runST $ do
    let w :: Num a => a
        w = 3
        straws = V.create $ do
            v <- MV.new $ V.length points
            let lastValidIndex = V.length points - 1

            MV.write v 1 $ distance (points V.! 0) (points V.! (1 + w)) * (2 * w) / (w + 1)
            MV.write v 2 $ distance (points V.! 0) (points V.! (2 + w)) * (2 * w) / (w + 2)
            MV.write v (lastValidIndex - 1) $ distance (points V.! lastValidIndex) (points V.! (lastValidIndex - 1 - w)) * (2 * w) / (w + 1)
            MV.write v (lastValidIndex - 2) $ distance (points V.! lastValidIndex) (points V.! (lastValidIndex - 2 - w)) * (2 * w) / (w + 2)

            forM_ [w .. lastValidIndex - w] $ \i -> do
                MV.write v i $ distance (points V.! (i - w)) (points V.! (i + w))

            return v

        t = mean straws * 0.95

    i <- newSTRef w
    corners <- newSTRef [0]

    whileM_ ((\x -> x < V.length points - w - 1) <$> readSTRef i) $ do
        iVal <- readSTRef i
        if straws V.! iVal < t
        then do
            localMin <- newSTRef $ 1 / 0
            localMinIndex <- newSTRef =<< readSTRef i

            let cond = do
                    iVal <- readSTRef i
                    return $ iVal < V.length points - w && straws V.! iVal < t

            whileM_ cond $ do
                iVal <- readSTRef i
                localMinVal <- readSTRef localMin
                when (straws V.! iVal < localMinVal) $ do
                    writeSTRef localMin $ straws V.! iVal
                    writeSTRef localMinIndex iVal

                increment i

            localMinIndexVal <- readSTRef localMinIndex
            modifySTRef corners (localMinIndexVal:)
        else increment i

    modifySTRef corners (V.length points - 1 :)

    cornersVal <- reverse <$> readSTRef corners

    return $ postProcessCorners (V.zipWith (,) points straws) (V.fromList cornersVal)


mean :: (Num a, Storable a, Fractional a) => V.Vector a -> a
mean vec = V.sum vec / (fromIntegral $ V.length vec)

iStraw :: V.Vector (V2 Float, Float) -> V.Vector (V2 Float)
iStraw (V.map fst -> points) = V.backpermute points corners
    where
        s = determineResampleSpacing points
        resampled = resample s points
        corners = getCorners resampled
