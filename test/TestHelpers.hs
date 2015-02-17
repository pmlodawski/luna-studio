{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs#-}

module TestHelpers where 

import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Flowbox.Prelude as P
import Flowbox.Graphics.Image.Image
import qualified Flowbox.Math.Matrix as M
import Flowbox.Graphics.Mockup.Basic as Mock
import Flowbox.Graphics.Mockup.Merge
import Data.Array.Accelerate.CUDA as AC
import Flowbox.Graphics.Composition.Merge

shouldBeCloseTo :: (Show a, Comparable a b) => String -> b -> a -> a -> Expectation
shouldBeCloseTo name metric actual expected = assertAlmostEqual name "" metric expected actual


returnShouldBeCloseTo testPath metric actual expected = do
    actual' <- actual
    expected' <- expected
    shouldBeCloseTo testPath metric actual' expected'



--assertAlmostEqual :: (Comparable a b, Show a) => String -- ^ The message prefix 
--                              -> String --test name
--                              -> b
--                              -> a      -- ^ The expected value 
--                              -> a      -- ^ The actual value
--                              -> Assertion
--assertAlmostEqual name preface metric expected actual =
--  unless (closeEnough metric actual expected) (assertFailure msg)
-- where msg = (if null preface then "" else preface ++ "\n") ++ (diffMsg name metric actual expected)
             --"expected close to: " ++ show expected ++ "\nbut got: " ++ show actual ++
             --"\ndifference: " 

--monadic
assertAlmostEqual :: (Comparable a b, Show a) => String -- ^ The message prefix 
                              -> String --test name
                              -> b
                              -> a      -- ^ The expected value 
                              -> a      -- ^ The actual value
                              -> Assertion
assertAlmostEqual name preface metric expected actual = do
    
    let ret = do
          ioMsg <- diffMsg name metric actual expected
          assertFailure $ (if null preface then "" else preface ++ "\n") ++ ioMsg
        in unless (closeEnough metric actual expected) ret
        --where msg = (if null preface then "" else preface ++ "\n") ++ ioMsg


class Comparable thing metric where 
    closeEnough :: metric -> thing -> thing -> Bool
    diffMsg     :: String -> metric -> thing -> thing -> IO String

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

    diffMsg _ _ Nothing Nothing = return "Nothings"
    diffMsg _ Exact (Just x) (Just y)     = return $ "expected: " ++ show (Just y) ++ 
                                          "\nbut got: " ++ show (Just x) ++
                                          "\ndifference: " ++ show (x-y)
    diffMsg _ (Close a) (Just x) (Just y) = return $ "expected max difference " ++ (show a) ++ " to: " ++ show (Just y) ++ 
                                          "\nbut got: " ++ show (Just x) ++
                                          "\ndifference: " ++ show (x-y)
    diffMsg _ _ _ _ = return "Nothing with Just"


data ImageMetric = PixelWise | TileWise | ImageWise deriving (Bounded, Enum)

instance Arbitrary ImageMetric where
    arbitrary = arbitraryBoundedEnum

instance Comparable Image ImageMetric where
    closeEnough metric actualImage expectedImage = (case metric of
        PixelWise -> maxDiff < 0.01 --imgAsList actualImage == imgAsList expectedImage
        TileWise  -> True
        ImageWise -> sC/sRefC < 0.01) where
            [maxDiff] = M.toList AC.run maxDif
            [sC] = M.toList AC.run s
            [sRefC] = M.toList AC.run sRef
            s = M.zipWith4 (\r g b a -> r+g+b+a) (M.sum r) (M.sum g) (M.sum b) (M.sum a)
            sRef = M.zipWith4 (\r g b a -> r+g+b+a) (M.sum r2) (M.sum g2) (M.sum b2) (M.sum a2)

            --(r,g,b,a) = Mock.unsafeGetChannels $ mergeLuna Difference Adobe actualImage expectedImage
            maxDif = M.zipWith4 (\r g b a -> maximum [r,g,b,a]) (M.maximum r) (M.maximum g) (M.maximum b) (M.maximum a)--maximum dif
            r = M.map abs $ M.zipWith (-) r1 r2
            g = M.map abs $ M.zipWith (-) g1 g2
            b = M.map abs $ M.zipWith (-) b1 b2
            a = M.map abs $ M.zipWith (-) a1 a2

            (r1,g1,b1,a1) = Mock.unsafeGetChannels actualImage
            (r2,g2,b2,a2) = Mock.unsafeGetChannels expectedImage
            


    diffMsg path metric actualImage expectedImage  = case metric of
        PixelWise ->    let (r1,g1,b1,a1) = Mock.unsafeGetChannels actualImage
                            (r2,g2,b2,a2) = Mock.unsafeGetChannels expectedImage
                          --(r,g,b,a) = Mock.unsafeGetChannels $ diff
                            

                            r = M.map abs $ M.zipWith (-) r1 r2
                            g = M.map abs $ M.zipWith (-) g1 g2
                            b = M.map abs $ M.zipWith (-) b1 b2
                            a = M.map abs $ M.zipWith (-) a1 a2

                            maxDif = M.zipWith4 (\r g b a -> maximum [r,g,b,a]) (M.maximum r) (M.maximum g) (M.maximum b) (M.maximum a)--maximum dif


                            resultPath = path++"Test/test_result.png"
                            diffPath = path++"Test/test_diff.png"
                            [maxDiff] = M.toList AC.run maxDif


                            diff = mergeLuna Difference Adobe actualImage expectedImage


                            finMsg = "wrong result saved to"++resultPath++"\ndiff image saved to "++diffPath ++ "\nmax pixel-wise difference: " ++ (show $ maxDiff)   
                            in do Mock.saveImageLuna resultPath actualImage       
                                    --return "diff image saved to ./samples/diff.png"
                                  Mock.saveImageLuna diffPath diff   
                                  return finMsg                
                        --return $ finMsg
                        --"pixel-wise difference" ++
                     --"actualImage sum: " ++ (show $ s1) ++
                     --"\nexpectedImage sum: " ++ (show $ s2) ++
                         --where
                          --  resultPath = name++"Test/test_result.png"
                          --  diffPath = name++"Test/test_diff.png"
                          --  [maxDiff] = M.toList AC.run maxDif
                          ----(r,g,b,a) = Mock.unsafeGetChannels $ diff
                          --  maxDif = M.zipWith4 (\r g b a -> maximum [r,g,b,a]) (M.maximum r) (M.maximum g) (M.maximum b) (M.maximum a)--maximum dif

                          --  r = M.map abs $ M.zipWith (-) r1 r2
                          --  g = M.map abs $ M.zipWith (-) g1 g2
                          --  b = M.map abs $ M.zipWith (-) b1 b2
                          --  a = M.map abs $ M.zipWith (-) a1 a2

                          --  (r1,g1,b1,a1) = Mock.unsafeGetChannels actualImage
                          --  (r2,g2,b2,a2) = Mock.unsafeGetChannels expectedImage

                          --  diff = mergeLuna Difference Adobe actualImage expectedImage
                     --   l1 = imgAsList actualImage 
                     --   l2 = imgAsList expectedImage
                     --   s1 = sum l1
                     --   s2 = sum l2
                     --   dif= zipWith (-) l1 l2
                     --   maxDif = maximum $ P.map abs dif
        TileWise  -> return "tile-wise difference"
        ImageWise -> return $ "actualImage sum: " ++ (show $ s1out) ++
                         "\nexpectedImage sum: " ++ (show $ s2out) ++
                         "\nimage-wise difference: " ++ (show $ sdout) where

                            [s1out] = M.toList AC.run s1
                            [s2out] = M.toList AC.run s2
                            [sdout] = M.toList AC.run sd
                            s1 = M.zipWith4 (\r g b a -> r+g+b+a) (M.sum r1) (M.sum g1) (M.sum b1) (M.sum a1)
                            s2 = M.zipWith4 (\r g b a -> r+g+b+a) (M.sum r2) (M.sum g2) (M.sum b2) (M.sum a2)
                            sd = M.zipWith4 (\r g b a -> r+g+b+a) (M.sum rd) (M.sum gd) (M.sum bd) (M.sum ad)

                            rd = M.map abs $ M.zipWith (-) r1 r2
                            gd = M.map abs $ M.zipWith (-) g1 g2
                            bd = M.map abs $ M.zipWith (-) b1 b2
                            ad = M.map abs $ M.zipWith (-) a1 a2
                            (r1,g1,b1,a1) = Mock.unsafeGetChannels actualImage
                            (r2,g2,b2,a2) = Mock.unsafeGetChannels expectedImage
                        --(rd,gd,bd,ad) = Mock.unsafeGetChannels $ mergeLuna Difference Adobe actualImage expectedImage

                        --l1 = imgAsList actualImage 
                        --l2 = imgAsList expectedImage
                        --s1 = M.sum l1
                        --s2 = sum l2
                        --dif= zipWith (-) l1 l2
                        --s  = sum $ P.map abs dif


--imgAsList img = 
--    let (r,g,b,a) = Mock.unsafeGetChannels img
--    in  (M.toList AC.run r) ++ (M.toList AC.run g) ++ (M.toList AC.run b) ++(M.toList AC.run a)

getDefaultTestPic specPath testName = 
    loadImageLuna $ specPath++testName++"Test/"++testName++"_expected.png"