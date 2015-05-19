{-# LANGUAGE ViewPatterns #-}

module Flowbox.GuiMockup.ShortStraw
    ( shortStraw
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


shortStraw :: V.Vector (V2 Float) -> V.Vector (V2 Float)
shortStraw points = V.backpermute resampled cornersIndices
    where
        s = determineResampleSpacing points
        resampled = resample s points
        cornersIndices = getCorners resampled

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

getCorners :: V.Vector (V2 Float) -> V.Vector Int
getCorners points = runST $ do
    let w = 3
        pointsLength = V.length points

    corners <- newSTRef [0]
    let straws = V.create $ do
            s <- MV.new $ pointsLength - 2 * w

            forM_ [w .. pointsLength - w] $ \i ->
                MV.write s i $ distance (points V.! (i - w)) (points V.! (i + w))

            return s

    let t = median straws * 0.95

    i <- newSTRef w

    whileM_ (readSTRef i >>= \x -> return $ x < pointsLength - w) $ do
        condition <- readSTRef i >>= \x -> return $ straws V.! x < t
        if condition
        then do
            localMin <- newSTRef $ 1 / 0
            localMinIndex <- readSTRef i >>= \x -> newSTRef x

            whileM_ (readSTRef i >>= \x -> return $ x < V.length straws && straws V.! x < t) $ do
                newMininumCondition <- do
                    i' <- readSTRef i
                    min <- readSTRef localMin
                    return $ straws V.! i' < min
                when newMininumCondition $ do
                    i' <- readSTRef i
                    writeSTRef localMin $ straws V.! i'
                    writeSTRef localMinIndex i'

                readSTRef i >>= \x -> writeSTRef i (x + 1)

            minIndex <- readSTRef localMinIndex
            readSTRef corners >>= \x -> writeSTRef corners (minIndex:x)
        else
            readSTRef i >>= \x -> writeSTRef i (x + 1)

    modifySTRef corners ((:) pointsLength)

    corners' <- readSTRef corners
    return $ V.fromList $ postProcessCorners points (reverse corners') straws

insertAt :: Int -> a -> [a] -> [a]
insertAt n a l = let (ys, zs) = splitAt n l in ys ++ [a] ++ zs

removeAt :: Int -> [a] -> [a]
removeAt n l = case back of
    []     -> error "removeAt: index too large"
    (_:rest) -> front ++ rest
    where
        (front, back) = splitAt (n-1) l

remove :: Eq a => a -> [a] -> [a]
remove toDelete l = [ x | x <- l, x /= toDelete]

postProcessCorners :: V.Vector (V2 Float) -> [Int] -> V.Vector Float -> [Int]
postProcessCorners points corners straws = runST $ do
    continue <- newSTRef False
    cornersRef <- newSTRef corners

    whileM (not <$> readSTRef continue) $ do
        writeSTRef continue True

        i <- newSTRef 1

        let cond = do
                iVal <- readSTRef i
                cornersVal <- readSTRef cornersRef
                return $ iVal < length cornersVal

        whileM_ cond $ do
            iVal <- readSTRef i
            c1 <- readSTRef cornersRef <&> \x -> x !! (iVal - 1)
            c2 <- readSTRef cornersRef <&> \x -> x !! iVal

            when (not $ isLine points c1 c2) $ do
                let newCorner = halfwayCorner straws c1 c2
                modifySTRef cornersRef $ insertAt iVal newCorner
                writeSTRef continue False

            increment i

    i <- newSTRef (1::Int)
    let cond = do
            iVal <- readSTRef i
            cornersVal <- readSTRef cornersRef
            return $ iVal < length cornersVal - 1

    whileM_ cond $ do
        iVal <- readSTRef i
        c1 <- readSTRef cornersRef <&> \x -> x !! (iVal - 1)
        c2 <- readSTRef cornersRef <&> \x -> x !! (iVal + 1)

        if isLine points c1 c2
        then do
            invalidCorner <- readSTRef cornersRef <&> \x -> x !! iVal
            modifySTRef cornersRef $ remove invalidCorner
            decrement i
        else
            increment i

    readSTRef cornersRef

halfwayCorner :: V.Vector Float -> Int -> Int -> Int
halfwayCorner = undefined

increment :: Num a => STRef s a -> ST s ()
increment ref = modifySTRef ref (+1)

decrement :: Num a => STRef s a -> ST s ()
decrement ref = modifySTRef ref (subtract 1)

isLine :: V.Vector (V2 Float) -> Int -> Int -> Bool
isLine points p1 p2 = dist / pathDist > threshold
    where
        threshold = 0.95
        dist = distance (points V.! p1) (points V.! p2)
        pathDist = pathDistance points p1 p2

pathDistance :: V.Vector (V2 Float) -> Int -> Int -> Float
pathDistance points i1 i2 = runST $ do
    d <- newSTRef 0

    forM_ [i1..i2] $ \i -> do
        d' <- readSTRef d
        writeSTRef d $ d' + distance (points V.! i) (points V.! (i + 1))

    readSTRef d

median :: (Num a, Ord a, Storable a, Fractional a) => V.Vector a -> a
median vec' = runST $ do
    vec <- V.thaw vec'
    VA.sort vec
    let len = MV.length vec
        ix = len `div` 2
    if even len
    then do
        x1 <- MV.read vec (ix-1)
        x2 <- MV.read vec ix
        return $ (x1 + x2) / 2
    else
        MV.read vec ix

resample :: Float -> V.Vector (V2 Float) -> V.Vector (V2 Float)
resample i v@(V.toList -> points) = V.fromList $ go 0 (head points) (tail points) [head points]
    where
        go :: Float -> V2 Float -> [V2 Float] -> [V2 Float] -> [V2 Float]
        go bigD previousPoint (point:ps) newPoints =
            let d = traceShowId $ distance previousPoint point
            in  if bigD + d >= i
                then let q = previousPoint + ((i - bigD) / d) *^ (point - previousPoint)
                     in  go 0 q (point:ps) (q:newPoints)
                else go (bigD + d) point ps newPoints
        go _ _ [] newPoints = reverse newPoints

pathLength :: V.Vector (V2 Float) -> Float
pathLength v = V.foldl' (\d (prev, next) -> d + distance prev next) 0
             $ V.zipWith (,) v (V.tail v)

test3 :: Float -> [[Float]] -> String
test3 n input = jsifyVector jsifyV2 $ resample n $ readPoints input
