module Luna.Typechecker.AlphaEquivSpec (spec) where


import Luna.Typechecker.AlphaEquiv  (AlphaEquiv(..), evalAlphaEquiv)
import Luna.Typechecker.Data.TVar   (TVar(..))
import Luna.Typechecker.Data.Type   (Type(..))

import Test.Hspec.LunaTypechecker


isEquivOf, isNotEquivOf :: (AlphaEquiv a) => a -> a -> Expectation
isEquivOf    x y = evalAlphaEquiv x y `shouldBe` True
isNotEquivOf x y = evalAlphaEquiv x y `shouldBe` False


spec :: Spec
spec = do
  describe "instance AlphaEquiv Type" $ do
    describe "trivial tests" $ do
        specify "trivial test 1" $
                        (TV$TVar 1)
            `isEquivOf` (TV$TVar 1)
        specify "trivial test 2" $
                        (TV$TVar 2)
            `isEquivOf` (TV$TVar 1)

    describe "functions" $ do
        specify "functions test  0a" $ 
                            ((TV$TVar 1) `Fun` (TV$TVar 1))
            `isEquivOf`     ((TV$TVar 1) `Fun` (TV$TVar 1))
        specify "functions test  1a" $ 
                            ((TV$TVar 1) `Fun` (TV$TVar 1))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (TV$TVar 0))
        specify "functions test  2a" $ 
                            ((TV$TVar 1) `Fun` (TV$TVar 1))
            `isNotEquivOf`  ((TV$TVar 0) `Fun` (TV$TVar 1))
        specify "functions test  3a" $ 
                            ((TV$TVar 1) `Fun` (TV$TVar 1))
            `isEquivOf`     ((TV$TVar 0) `Fun` (TV$TVar 0))
        specify "functions test  4a" $ 
                            ((TV$TVar 1) `Fun` (TV$TVar 0))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (TV$TVar 1))
        specify "functions test  5a" $ 
                            ((TV$TVar 1) `Fun` (TV$TVar 0))
            `isEquivOf`     ((TV$TVar 1) `Fun` (TV$TVar 0))
        specify "functions test  6a" $ 
                            ((TV$TVar 1) `Fun` (TV$TVar 0))
            `isEquivOf`     ((TV$TVar 0) `Fun` (TV$TVar 1))
        specify "functions test  7a" $ 
                            ((TV$TVar 1) `Fun` (TV$TVar 0))
            `isNotEquivOf`  ((TV$TVar 0) `Fun` (TV$TVar 0))
        specify "functions test  8a" $ 
                            ((TV$TVar 0) `Fun` (TV$TVar 1))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (TV$TVar 1))
        specify "functions test  9a" $ 
                            ((TV$TVar 0) `Fun` (TV$TVar 1))
            `isEquivOf`     ((TV$TVar 1) `Fun` (TV$TVar 0))
        specify "functions test 10a" $ 
                            ((TV$TVar 0) `Fun` (TV$TVar 1))
            `isEquivOf`     ((TV$TVar 0) `Fun` (TV$TVar 1))
        specify "functions test 11a" $ 
                            ((TV$TVar 0) `Fun` (TV$TVar 1))
            `isNotEquivOf`  ((TV$TVar 0) `Fun` (TV$TVar 0))
        specify "functions test 12a" $ 
                            ((TV$TVar 0) `Fun` (TV$TVar 0))
            `isEquivOf`     ((TV$TVar 1) `Fun` (TV$TVar 1))
        specify "functions test 13a" $ 
                            ((TV$TVar 0) `Fun` (TV$TVar 0))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (TV$TVar 0))
        specify "functions test 14a" $ 
                            ((TV$TVar 0) `Fun` (TV$TVar 0))
            `isNotEquivOf`  ((TV$TVar 0) `Fun` (TV$TVar 1))
        specify "functions test 15a" $ 
                            ((TV$TVar 0) `Fun` (TV$TVar 0))
            `isEquivOf`     ((TV$TVar 0) `Fun` (TV$TVar 0))
        specify "functions test  0b" $ 
                            ((TV$TVar 8) `Fun` (TV$TVar 8))
            `isEquivOf`     ((TV$TVar 1) `Fun` (TV$TVar 1))
        specify "functions test  1b" $ 
                            ((TV$TVar 8) `Fun` (TV$TVar 8))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (TV$TVar 0))
        specify "functions test  2b" $ 
                            ((TV$TVar 8) `Fun` (TV$TVar 8))
            `isNotEquivOf`  ((TV$TVar 0) `Fun` (TV$TVar 1))
        specify "functions test  3b" $ 
                            ((TV$TVar 8) `Fun` (TV$TVar 8))
            `isEquivOf`     ((TV$TVar 0) `Fun` (TV$TVar 0))
        specify "functions test  4b" $ 
                            ((TV$TVar 8) `Fun` (TV$TVar 9))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (TV$TVar 1))
        specify "functions test  5b" $ 
                            ((TV$TVar 8) `Fun` (TV$TVar 9))
            `isEquivOf`     ((TV$TVar 1) `Fun` (TV$TVar 0))
        specify "functions test  6b" $ 
                            ((TV$TVar 8) `Fun` (TV$TVar 9))
            `isEquivOf`     ((TV$TVar 0) `Fun` (TV$TVar 1))
        specify "functions test  7b" $ 
                            ((TV$TVar 8) `Fun` (TV$TVar 9))
            `isNotEquivOf`  ((TV$TVar 0) `Fun` (TV$TVar 0))
        specify "functions test  8b" $ 
                            ((TV$TVar 9) `Fun` (TV$TVar 8))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (TV$TVar 1))
        specify "functions test  9b" $ 
                            ((TV$TVar 9) `Fun` (TV$TVar 8))
            `isEquivOf`     ((TV$TVar 1) `Fun` (TV$TVar 0))
        specify "functions test 10b" $ 
                            ((TV$TVar 9) `Fun` (TV$TVar 8))
            `isEquivOf`     ((TV$TVar 0) `Fun` (TV$TVar 1))
        specify "functions test 11b" $ 
                            ((TV$TVar 9) `Fun` (TV$TVar 8))
            `isNotEquivOf`  ((TV$TVar 0) `Fun` (TV$TVar 0))
        specify "functions test 12b" $ 
                            ((TV$TVar 9) `Fun` (TV$TVar 9))
            `isEquivOf`     ((TV$TVar 1) `Fun` (TV$TVar 1))
        specify "functions test 13b" $ 
                            ((TV$TVar 9) `Fun` (TV$TVar 9))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (TV$TVar 0))
        specify "functions test 14b" $ 
                            ((TV$TVar 9) `Fun` (TV$TVar 9))
            `isNotEquivOf`  ((TV$TVar 0) `Fun` (TV$TVar 1))
        specify "functions test 15b" $ 
                            ((TV$TVar 9) `Fun` (TV$TVar 9))
            `isEquivOf`     ((TV$TVar 0) `Fun` (TV$TVar 0))

    describe "records" $ do
        specify "records test 1" $
                            Record [("a",TV$TVar 0), ("b",TV$TVar 1), ("c",TV$TVar 2)]
            `isEquivOf`     Record [("x",TV$TVar 7), ("y",TV$TVar 8), ("z",TV$TVar 9)]
        specify "records test 2" $
                            Record [("a",TV$TVar 0), ("b",TV$TVar 1), ("c",TV$TVar 2)]
            `isEquivOf`     Record [("a",TV$TVar 0), ("b",TV$TVar 1), ("c",TV$TVar 2)]
        specify "records test 3" $
                            Record [("a",TV$TVar 0), ("b",TV$TVar 1), ("c",TV$TVar 2)]
            `isNotEquivOf`  Record [("x",TV$TVar 7)]
        specify "records test 4" $
                            Record [("a",TV$TVar 0), ("b",TV$TVar 1), ("c",TV$TVar 2)]
            `isNotEquivOf`  Record []
        specify "records test 5" $
                            Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 2)]
            `isEquivOf`     Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)]

    describe "records and functions" $ do
        specify "mixed records & functions 1" $
                            ((TV$TVar 8) `Fun` (Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 2)]))
            `isEquivOf`     ((TV$TVar 1) `Fun` (Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)]))
        specify "mixed records & functions 2" $
                            ((TV$TVar 8) `Fun` (Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 2)]) `Fun` (TV$TVar 9))
            `isEquivOf`     ((TV$TVar 1) `Fun` (Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)]) `Fun` (TV$TVar 0))
        specify "mixed records & functions 2 reorder" $
                            ((TV$TVar 8) `Fun` (Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 2)]) `Fun` (TV$TVar 9))
            `isEquivOf`     ((TV$TVar 1) `Fun` (Record [("z",TV$TVar 9), ("y",TV$TVar 8), ("x",((TV$TVar 1) `Fun` (TV$TVar 0)))]) `Fun` (TV$TVar 0))
        specify "mixed records & functions 3" $
                            (Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 8)])
            `isNotEquivOf`  (Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)])
        specify "mixed records & functions 4" $
                            ((TV$TVar 8) `Fun` (Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 1)]))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)]))
        specify "mixed records & functions 5" $
                            ((TV$TVar 8) `Fun` (Record [("a",((TV$TVar 1) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 2)]) `Fun` (TV$TVar 9))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)]) `Fun` (TV$TVar 0))
        specify "mixed records & functions 5 reorder" $
                            ((TV$TVar 8) `Fun` (Record [("a",((TV$TVar 1) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 2)]) `Fun` (TV$TVar 9))
            `isNotEquivOf`  ((TV$TVar 1) `Fun` (Record [("y",TV$TVar 8), ("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("z",TV$TVar 9)]) `Fun` (TV$TVar 0))
        specify "mixed records & functions 6" $
                            ((TV$TVar 0) `Fun` (Record [("foo", (TV$TVar 0) `Fun` (TV$TVar 1))]) `Fun` (TV$TVar 1))
            `isEquivOf`     ((TV$TVar 0) `Fun` (Record [("foo", (TV$TVar 0) `Fun` (TV$TVar 1))]) `Fun` (TV$TVar 1))

