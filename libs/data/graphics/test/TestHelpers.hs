{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs#-}

module TestHelpers where 

import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Flowbox.Prelude as P
import Flowbox.Graphics.Image.Image
import qualified Flowbox.Math.Matrix as M
import Flowbox.Graphics.Mockup as Mock
import Data.Array.Accelerate.CUDA as AC
import System.IO.Unsafe
import Flowbox.Graphics.Composition.Merge

shouldBeCloseTo :: (Show a, Comparable a b) => b -> a -> a -> Expectation
shouldBeCloseTo metric a b = assertAlmostEqual "" metric b a


assertAlmostEqual :: (Comparable a b, Show a) => String -- ^ The message prefix 
                              -> b
                              -> a      -- ^ The expected value 
                              -> a      -- ^ The actual value
                              -> Assertion
assertAlmostEqual preface metric expected actual =
  unless (closeEnough metric actual expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++ (diffMsg metric actual expected)
             --"expected close to: " ++ show expected ++ "\nbut got: " ++ show actual ++
             --"\ndifference: " 


class Comparable thing metric where 
    closeEnough :: metric -> thing -> thing -> Bool
    diffMsg     :: metric -> thing -> thing -> String

--instance (Eq a) => Comparable (Maybe a) where
--    Nothing `close` Nothing = True
--    Just x  `close` Just y  = x == y
--    _ `close` _ = False

data FloatMetric a where 
    Exact :: FloatMetric a
    Close :: Floating a => a -> FloatMetric a

instance (Show a, Ord a, Floating a) => Comparable (Maybe a) (FloatMetric a) where
    closeEnough _ Nothing Nothing = True
    closeEnough Exact (Just x) (Just y) = x==y
    closeEnough (Close a) (Just x) (Just y) = (x+a >= y) && (x-a <= y)
    closeEnough _ _ _ = False

    diffMsg _ Nothing Nothing = "Nothings"
    diffMsg Exact (Just x) (Just y)     = "expected: " ++ show (Just y) ++ 
                                          "\nbut got: " ++ show (Just x) ++
                                          "\ndifference: " ++ show (x-y)
    diffMsg (Close a) (Just x) (Just y) = "expected max difference " ++ (show a) ++ " to: " ++ show (Just y) ++ 
                                          "\nbut got: " ++ show (Just x) ++
                                          "\ndifference: " ++ show (x-y)
    diffMsg _ _ _ = "Nothing with Just"


data ImageMetric = PixelWise | TileWise | ImageWise deriving (Bounded, Enum)

instance Arbitrary ImageMetric where
    arbitrary = arbitraryBoundedEnum

instance Comparable Image ImageMetric where
    closeEnough metric actualImage expectedImage = (case metric of
        PixelWise -> maxDiff < 0.93 --imgAsList actualImage == imgAsList expectedImage
        TileWise  -> True
        ImageWise -> sbool <50) where
            [maxDiff] = M.toList AC.run maxDif
            [sbool] = M.toList AC.run s
            s = M.zipWith4 (\r g b a -> r+g+b+a) (M.sum r) (M.sum g) (M.sum b) (M.sum a)

            (r,g,b,a) = Mock.unsafeGetChannels $ mergeLuna Difference Adobe actualImage expectedImage
            maxDif = M.zipWith4 (\r g b a -> r+g+b+a) (M.maximum r) (M.maximum g) (M.maximum b) (M.maximum a)--maximum dif
            


    diffMsg metric actualImage expectedImage  = case metric of
        PixelWise -> (unsafePerformIO $ do
                        let diff =  mergeLuna Difference Adobe actualImage expectedImage
                        Mock.saveImageLuna "./samples/diff.png" diff        
                        return "diff image saved to ./samples/diff.png")
                        --"pixel-wise difference" ++
                     --"actualImage sum: " ++ (show $ s1) ++
                     --"\nexpectedImage sum: " ++ (show $ s2) ++
                     ++ "\nmax pixel-wise difference: " ++ (show $ maxDiff) where
                    [maxDiff] = M.toList AC.run maxDif
                    (r,g,b,a) = Mock.unsafeGetChannels $ mergeLuna Difference Adobe actualImage expectedImage
                    maxDif = M.zipWith4 (\r g b a -> r+g+b+a) (M.maximum r) (M.maximum g) (M.maximum b) (M.maximum a)--maximum dif
                     --   l1 = imgAsList actualImage 
                     --   l2 = imgAsList expectedImage
                     --   s1 = sum l1
                     --   s2 = sum l2
                     --   dif= zipWith (-) l1 l2
                     --   maxDif = maximum $ P.map abs dif
        TileWise  -> "tile-wise difference"
        ImageWise -> "actualImage sum: " ++ (show $ s1out) ++
                     "\nexpectedImage sum: " ++ (show $ s2out) ++
                     "\nimage-wise difference: " ++ (show $ sdout) where

                        [s1out] = M.toList AC.run s1
                        [s2out] = M.toList AC.run s2
                        [sdout] = M.toList AC.run sd
                        s1 = M.zipWith4 (\r g b a -> r+g+b+a) (M.sum r1) (M.sum g1) (M.sum b1) (M.sum a1)
                        s2 = M.zipWith4 (\r g b a -> r+g+b+a) (M.sum r2) (M.sum g2) (M.sum b2) (M.sum a2)
                        sd = M.zipWith4 (\r g b a -> r+g+b+a) (M.sum rd) (M.sum gd) (M.sum bd) (M.sum ad)
                        (r1,g1,b1,a1) = Mock.unsafeGetChannels actualImage
                        (r2,g2,b2,a2) = Mock.unsafeGetChannels expectedImage
                        (rd,gd,bd,ad) = Mock.unsafeGetChannels $ mergeLuna Difference Adobe actualImage expectedImage

                        --l1 = imgAsList actualImage 
                        --l2 = imgAsList expectedImage
                        --s1 = M.sum l1
                        --s2 = sum l2
                        --dif= zipWith (-) l1 l2
                        --s  = sum $ P.map abs dif


imgAsList img = 
    let (r,g,b,a) = Mock.unsafeGetChannels img
    in  (M.toList AC.run r) ++ (M.toList AC.run g) ++ (M.toList AC.run b) ++(M.toList AC.run a)