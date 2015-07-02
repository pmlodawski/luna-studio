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


shortStraw :: V.Vector (V2 Double) -> V.Vector (V2 Double)
shortStraw points = V.backpermute resampled cornersIndices
    where
        s = determineResampleSpacing points
        resampled = resample s points
        cornersIndices = getCorners resampled

determineResampleSpacing :: V.Vector (V2 Double) -> Double
determineResampleSpacing points = diagonal / 40
    where
        diagonal = distance topLeft bottomRight
        topLeft = V2 minX minY
        bottomRight = V2 maxX maxY

        minX = V.minimumBy (compare `on` (^. _x)) points ^. _x
        minY = V.minimumBy (compare `on` (^. _y)) points ^. _y
        maxX = V.maximumBy (compare `on` (^. _x)) points ^. _x
        maxY = V.maximumBy (compare `on` (^. _y)) points ^. _y

getCorners :: V.Vector (V2 Double) -> V.Vector Int
getCorners points = runST $ do
    let w = 3
        pointsLength = V.length points

    corners <- newSTRef [0]
    let straws = V.create $ do
            s <- MV.new pointsLength

            forM_ [w .. pointsLength - w - 1] $ \i ->
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

                increment i

            minIndex <- readSTRef localMinIndex
            modifySTRef corners (minIndex:)
        else
            increment i

    modifySTRef corners ((:) (pointsLength - 1))

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

postProcessCorners :: V.Vector (V2 Double) -> [Int] -> V.Vector Double -> [Int]
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
            c1 <- traceShow iVal $ readSTRef cornersRef <&> \x -> x !! (iVal - 1)
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
            -- decrement i
        else
            increment i

    readSTRef cornersRef

halfwayCorner :: V.Vector Double -> Int -> Int -> Int
halfwayCorner straws a b = runST $ do
    minValue <- newSTRef $ 1 / 0
    minIndex <- newSTRef 0

    let quarter = (b - a) `div` 4
    forM_ [a + quarter .. b - quarter] $ \i -> do
        cond <- (straws V.! i <) <$> readSTRef minValue
        when cond $ do
            writeSTRef minValue $ straws V.! i
            writeSTRef minIndex i

    readSTRef minIndex

increment :: Num a => STRef s a -> ST s ()
increment ref = modifySTRef ref (+1)

decrement :: Num a => STRef s a -> ST s ()
decrement ref = modifySTRef ref (subtract 1)

isLine :: V.Vector (V2 Double) -> Int -> Int -> Bool
isLine points p1 p2 = dist / pathDist > threshold
    where
        threshold = 0.95
        dist = distance (points V.! p1) (points V.! p2)
        pathDist = pathDistance points p1 p2

pathDistance :: V.Vector (V2 Double) -> Int -> Int -> Double
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

resample :: Double -> V.Vector (V2 Double) -> V.Vector (V2 Double)
resample i v@(V.toList -> points) = V.fromList $ go 0 (head points) (tail points) [head points]
    where
        go :: Double -> V2 Double -> [V2 Double] -> [V2 Double] -> [V2 Double]
        go bigD previousPoint (point:ps) newPoints =
            let d = distance previousPoint point
            in  if bigD + d >= i
                then let q = previousPoint + ((i - bigD) / d) *^ (point - previousPoint)
                     in  go 0 q (point:ps) (q:newPoints)
                else go (bigD + d) point ps newPoints
        go _ _ [] newPoints = reverse newPoints

resampleScala :: Double -> V.Vector (V2 Double) -> V.Vector (V2 Double)
resampleScala i v@(V.toList -> points) = V.reverse $ V.fromList $ resamplePoints (reverse points) [] 0
    where
        resamplePoints :: [V2 Double] -> [V2 Double] -> Double -> [V2 Double]
        resamplePoints [] lz _ = lz
        resamplePoints (h:t) lz d = case t of
            [_] -> lz ++ [h]
            _   -> let pt1 = h
                       pt2 = head t
                       d' = distance pt1 pt2
                   in if d + d' >= i
                      then let q = pt1 + ((i - d) / d') *^ (pt2 - pt1)
                           in  resamplePoints (q:t) (lz ++ [q]) 0
                      else resamplePoints t lz (d + d')

pathLength :: V.Vector (V2 Double) -> Double
pathLength v = V.foldl' (\d (prev, next) -> d + distance prev next) 0
             $ V.zipWith (,) v (V.tail v)

test3 :: Double -> [[Double]] -> String
test3 n input = jsifyVector jsifyV2 $ resample n $ readPoints input


-- clean reimplementation

-- resample

-- determineResampleSpacing

halfwayCornerClean :: V.Vector (V2 Double, Double) -> V2 Double
halfwayCornerClean vec = fst $ V.minimumBy (compare `on` snd) vec'
    where
        vec'    = V.slice quarter len vec
        len     = V.length vec - 2 * quarter
        quarter = V.length vec `div` 4

postProcessCornersClean :: V.Vector (V2 Double, Double) -> V.Vector (V2 Double) -> V.Vector (V2 Double)
postProcessCornersClean points (V.toList -> corners) = V.fromList $ go3 (go1 False corners) []
    where
        go1 :: Bool -> [V2 Double] -> [V2 Double]
        go1 (not -> False) acc = acc
        go1 (not -> True)  acc = go2 True acc []
            where
                go2 :: Bool -> [V2 Double] -> [V2 Double] -> [V2 Double]
                go2 currentBool corners@(c1:c2:cs) acc =
                    let slice = getSlice points c1 c2
                    in  if not $ isLineClean $ V.map fst slice
                        then let newCorner = halfwayCornerClean slice
                             in  if newCorner > c1 && newCorner < c2
                                 then go2 False (newCorner:c2:cs) (c1:acc)
                                 else go2 currentBool (c2:cs) (c1:acc)
                        else go2 currentBool (c2:cs) (c1:acc)
                go2 currentBool [c]                acc = go1 currentBool (reverse $ c:acc)

        go3 :: [V2 Double] -> [V2 Double] -> [V2 Double]
        go3 (c1:c:c2:cs) acc = if isLineClean $ V.map fst $ getSlice points c1 c2
                               then go3 (c1:c2:cs) acc
                               else go3 (c:c2:cs) (c1:acc)
        go3 [c1,c2]      acc = reverse $ c2:c1:acc

        getSlice :: V.Vector (V2 Double, Double) -> V2 Double -> V2 Double -> V.Vector (V2 Double, Double)
        getSlice vec a1 a2 = V.slice i1 (i2 - i1 + 1) vec
            where
                fstvec = V.map fst vec
                Just i1 = V.elemIndex a1 fstvec
                Just i2 = V.elemIndex a2 fstvec

isLineClean :: V.Vector (V2 Double) -> Bool
isLineClean vec = len / pathLen > threshold
    where
        len = distance (V.head vec) (V.last vec)
        pathLen = pathLength vec
        threshold = 0.95

getCornersClean :: V.Vector (V2 Double) -> V.Vector (V2 Double)
getCornersClean points = runST $ do
    let w = 3
        straws = V.create $ do
            v <- MV.new $ V.length points

            forM_ [w .. V.length points - w - 1] $ \i -> do
                MV.write v i $ distance (points V.! (i - w)) (points V.! (i + w))

            return v

        t = median straws * 0.95

    i <- newSTRef w
    corners <- newSTRef [V.head points]

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
            modifySTRef corners (points V.! localMinIndexVal:)
        else increment i

    modifySTRef corners (V.last points:)

    cornersVal <- reverse <$> readSTRef corners

    return $ postProcessCornersClean (V.zipWith (,) points straws) (V.fromList cornersVal)

shortStrawClean :: V.Vector (V2 Double) -> V.Vector (V2 Double)
shortStrawClean points = corners
    where
        s = determineResampleSpacing points
        resampled = resample s points
        corners = getCornersClean resampled
