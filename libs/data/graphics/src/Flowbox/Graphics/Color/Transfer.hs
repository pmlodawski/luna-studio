{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Flowbox.Graphics.Color.Transfer where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as AIO
import Data.Array.Accelerate.CUDA

import           Flowbox.Graphics.Composition.Histogram
import qualified Flowbox.Graphics.Utils as U
import           Flowbox.Graphics.Utils.Accelerate
import qualified Flowbox.Math.Numeric   as Num
import           Flowbox.Prelude




rotation :: A.Acc (A.Array A.DIM2 Float)
rotation = A.map (/ sqrt 2) $ A.use $ A.fromList (A.Z A.:. 6 A.:. 3) [
       0.427019   , (-0.112186) ,  0.897256
    ,  0.886757   , (-0.142236) , (-0.439807)
    , (-0.176963) , (-0.983455) , (-0.038744)
    ,  0.934839   ,  0.158203   ,  0.317881
    , (-0.128881) , (-0.683015) ,  0.718944
    ,  0.330857   , (-0.713065) , (-0.618119)
    ]

bigG :: A.Acc (A.Array A.DIM2 Float) -> A.Acc (A.Array A.DIM2 Float)
bigG = Num.matMul rotation

bigR = Num.matMul rotation

--smin :: A.Acc (A.Vector Float)
smin h p = A.fold1 min $ bigG h A.++ bigR p

smax h p = A.fold1 max $ bigG h A.++ bigR p


--rho_gi :: A.Acc (A.Array A.DIM2 Float)
rho_gi h p = A.reshape (A.index2 6 (256 :: A.Exp Int))
     $ A.asnd
     $ A.awhile 
         (\v -> A.unit $ A.the (A.afst v) A.<* 6)
         rho_gi_step
         (A.lift (A.unit $ A.constant 0, emptyVector))
 where
   rho_gi_step :: A.Acc (A.Scalar Int, A.Vector Int) -> A.Acc (A.Scalar Int, A.Vector Int)
   rho_gi_step (A.unlift -> (it', vec) :: (A.Acc (A.Scalar Int), A.Acc (A.Vector Int))) =
     let currentIteration = A.the it'
 
         currentMin = smin h p A.!! currentIteration
         currentMax = smax h p A.!! currentIteration
 
         sliceG = A.slice (bigG h) (A.lift $ A.Z A.:. currentIteration A.:. A.All)
 
     in  A.lift (A.unit (currentIteration + 1),
                 vec A.++ (histogram' $ histogram currentMin currentMax 256 sliceG))

--histogram' :: forall a. (A.Elt a, A.IsFloating a) => Histogram a -> A.Acc (A.Vector a)
--histogram' (A.unlift -> (hist, _, _) :: Histogram' a) = normalizeHistogram hist
histogram' (A.unlift -> (hist, _, _) :: Histogram' a) = hist

normalizeHistogram :: (A.Elt e, A.IsFloating e) => A.Acc (A.Vector Int) -> A.Acc (A.Vector e)
normalizeHistogram hist = A.map (\x -> A.fromIntegral x / sum') hist
  where
    sum' = A.fromIntegral $ A.the $ A.sum hist


histogramWithBins' :: (A.Elt e, A.IsFloating e) => A.Exp e -> A.Exp e -> A.Exp Int -> A.Acc (A.Vector e) -> A.Acc (A.Vector Int)
histogramWithBins' mini maxi bins vec = A.permute (+) zeros hist ones
  where
    bins' = A.the $ A.unit $ bins
    step  = (maxi - mini) / (A.fromIntegral bins' - 1)

    zeros = A.fill (A.index1 bins') (A.constant 0 :: A.Exp Int)
    ones  = A.fill (A.shape vec)    (A.constant 1 :: A.Exp Int)

    hist ix = A.index1 (A.ceiling $ ((vec A.! ix) - mini) / step :: A.Exp Int)

szatan = do
  (plainR, plainG, plainB) <- loadRGBA "plain.bmp"
  (houseR, houseG, houseB) <- loadRGBA "house.bmp"
  putStrLn "loaded"

  let vectoredPlain = customReshape (A.lift $ (A.use plainR, A.use plainG, A.use plainB))
      vectoredHouse = customReshape (A.lift $ (A.use houseR, A.use houseG, A.use houseB))

  let !foo = flip A.slice (A.lift $ A.Z A.:. (0::A.Exp Int) A.:. A.All) $ rho_gi vectoredHouse vectoredPlain

  return foo

customReshape :: A.Acc (A.Array A.DIM2 Float, A.Array A.DIM2 Float, A.Array A.DIM2 Float) -> A.Acc (A.Array A.DIM2 Float)
customReshape (A.unlift -> (r, g, b) :: Vectors) = A.reshape newShape ((A.flatten r) A.++ (A.flatten g) A.++ (A.flatten b))
  where
    newShape = A.index2 3 (A.size r)

type Vectors = (A.Acc (A.Array A.DIM2 Float), A.Acc (A.Array A.DIM2 Float), A.Acc (A.Array A.DIM2 Float))


convert t = let (r, g, b, _) = A.unlift t :: (A.Exp A.Word8, A.Exp A.Word8, A.Exp A.Word8, A.Exp A.Word8)
            in  A.lift (A.fromIntegral r / 255, A.fromIntegral g / 255, A.fromIntegral b / 255)

loadRGBA :: FilePath -> IO (A.Array A.DIM2 Float, A.Array A.DIM2 Float, A.Array A.DIM2 Float)
loadRGBA file = do
  Right file <- AIO.readImageFromBMP file
  return $ run $ A.lift $ A.unzip3 $ A.map (convert . AIO.unpackRGBA32) $ A.use file
