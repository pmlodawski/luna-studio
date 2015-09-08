{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Math.Coordinate.Cartesian (Point2 (..))
import           Math.Space.Space

import           Flowbox.Graphics.Shader.Shader (Shader(..), DiscreteShader, ContinuousShader)
import qualified Flowbox.Graphics.Shader.Shader as Shader
import           Flowbox.Graphics.Utils.Accelerate (variable)
import qualified Flowbox.Math.Index as Index
import           Flowbox.Prelude

import           Data.Array.Accelerate
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as C



type AD2F = Acc (Array DIM2 Float)
type SF = DiscreteShader (Exp Float)
type F4 = (Float, Float, Float, Float)
type EF4 = (Exp Float, Exp Float, Exp Float, Exp Float)

type EDIM1 = A.DIM0 A.:. A.Exp Int
type EDIM2 = EDIM1 A.:. A.Exp Int
type EDIM3 = EDIM2 A.:. A.Exp Int
type EDIM4 = EDIM3 A.:. A.Exp Int

unsafeFromMatrix :: Elt e => Acc (Array DIM2 e) -> DiscreteShader (Exp e)
unsafeFromMatrix mat = Shader cnv $ \(Point2 x y) -> mat A.! A.index2 y x
    where Z :. h :. w = A.unlift $ shape mat :: EDIM2
          cnv = Grid w h

fromMatrix :: Elt e => Boundary (Exp e) -> Acc (Array DIM2 e) -> DiscreteShader (Exp e)
fromMatrix b mat = bound b $ unsafeFromMatrix mat

bound :: Elt t => Boundary (Exp t) -> DiscreteShader (Exp t) -> DiscreteShader (Exp t)
bound b gen = Shader (Shader.canvas gen) (Index.boundedIndex2D b gen)

rasterizer :: Elt e => DiscreteShader (Exp e) -> Acc (Array DIM2 e)
rasterizer (Shader (Grid width' height') gen) = A.generate (A.index2 height' width') wrapper
    where wrapper (A.unlift . A.unindex2 -> (y, x)) = gen (Point2 x y)


-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------

foldIt :: Acc (Array DIM2 F4) -> Acc (Scalar F4)
foldIt = A.fold1All (\(A.unlift -> (a1, b1, c1, d1) :: EF4) (A.unlift -> (a2, b2, c2, d2) :: EF4) -> A.lift (a1+a2, b1+b2, c1+c2, d1+d2))

mapOnZipped :: forall e. Elt e => (Exp e -> Exp e -> Exp e) -> Acc (Vector (e, e)) -> Acc (Vector e)
mapOnZipped f = A.map mapIt
    where mapIt (A.unlift -> (a, b) :: (Exp e, Exp e)) = f a b

rotF :: (Elt e, IsFloating e) => Exp e -> Exp e -> Exp e -> Exp e
rotF a x y = x * cos a - y * sin a

rotI :: (Elt e, Elt a, IsFloating e, IsIntegral a) => Exp e -> Exp a -> Exp a -> Exp a
rotI a x y = A.truncate $ A.fromIntegral x * cos a - A.fromIntegral y * sin a

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    let w = 4096 :: Int
        h = 3112 :: Int
        sh = w * h -- 12746752
        num = variable (5 :: Float)
        numI = variable (7 :: Int)
        numF = variable (7 :: Float)
        angle = variable (0.7 :: Float)

    let arrF = A.use $ [1..12746752] :: Acc (Vector Float)
        arrI = A.use $ [1..12746752] :: Acc (Vector Int)

    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    print "C.run $ A.fold1 (+) $ A.map (+num) arrI"
    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    print $ C.run $ A.fold1 (+) $ A.map (+numI) arrI
    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"

    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    print "C.run $ A.fold1 (+) $ A.map (+num) arrF"
    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    print $ C.run $ A.fold1 (+) $ A.map (+numF) arrF
    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"

    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    print "C.run $ A.fold1 (+) $ mapOnZipped (rotI angle) $ A.zip arrI arrI"
    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    print $ C.run $ A.fold1 (+) $ mapOnZipped (rotI angle) $ A.zip arrI arrI
    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"

    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    print "C.run $ A.fold1 (+) $ mapOnZipped (rotF angle) $ A.zip arrF arrF"
    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    print $ C.run $ A.fold1 (+) $ mapOnZipped (rotF angle) $ A.zip arrF arrF
    print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"

-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------


    --let arr1 = A.use $ A.fromList (Z:.h:.w) [1..12746752] :: AD2F
    --    arr2 = A.use $ A.fromList (Z:.h:.w) [1..12746752] :: AD2F
    --    arr3 = A.use $ A.fromList (Z:.h:.w) [1..12746752] :: AD2F
    --    arr4 = A.use $ A.fromList (Z:.h:.w) [1..12746752] :: AD2F


    --let arr = [arr1, arr2, arr3, arr4] :: [AD2F]
    --    arr' = fmap (A.map (subtract num)) arr


    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "fmap C.run arr'"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print $ fmap (C.run . A.sum) arr'
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"


    --let arrZ = A.zip4 arr1 arr2 arr3 arr4
    --    arrZ' = foldIt $ A.map (\(A.unlift -> (a, b, c, d) :: EF4) -> A.lift (a+num, b+num, c+num, d+num)) arrZ


    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "C.run arrZ'"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print $ C.run arrZ'
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"


    --let s1 = fromMatrix (Constant 0) arr1
    --    s2 = fromMatrix (Constant 0) arr2
    --    s3 = fromMatrix (Constant 0) arr3
    --    s4 = fromMatrix (Constant 0) arr4


    --let s = [s1, s2, s3, s4] :: [SF]
    --    s' = fmap (rasterizer . fmap (subtract num)) s


    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "fmap C.run s'"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print $ fmap (C.run . A.sum) s'
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"


    --let sZ = foldIt $ rasterizer $ (\f sh1 sh2 sh3 sh4 -> Shader (Shader.canvas sh1) (
    --             \p -> A.lift ( (fmap f $ Shader.runShader sh1) p
    --                          , (fmap f $ Shader.runShader sh2) p
    --                          , (fmap f $ Shader.runShader sh3) p
    --                          , (fmap f $ Shader.runShader sh4) p
    --                          ) -- :: Exp (Float, Float, Float, Float)
    --             )) (+num) s1 s2 s3 s4

    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "fmap C.run sZ"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print $ C.run sZ
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"
    --print "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -"


    return ()
