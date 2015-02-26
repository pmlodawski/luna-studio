module Luna.Typechecker.AlphaEquivSpec (spec) where


import Luna.Typechecker.AlphaEquiv      (AlphaEquiv(..), evalAlphaEquiv)
import Luna.Typechecker.Data.Constraint (Constraint(..))
import Luna.Typechecker.Data.Predicate  (Predicate(..))
import Luna.Typechecker.Data.TVar       (TVar(..))
import Luna.Typechecker.Data.Type       (Type(..))
import Luna.Typechecker.Data.TypeScheme (TypeScheme(..))

import Test.Hspec.LunaTypechecker



isEquivOf, isNotEquivOf :: (AlphaEquiv a) => a -> a -> Expectation
isEquivOf    x y = evalAlphaEquiv x y `shouldBe` True
isNotEquivOf x y = evalAlphaEquiv x y `shouldBe` False


spec :: Spec
spec = do
  describe "instance AlphaEquiv Predicate" $ do
      specify "trivial 1" $
                          TRUE
          `isEquivOf`     TRUE
      specify "trivial 2" $
                          (tp_0 `Subsume` tp_1)
          `isEquivOf`     (tp_0 `Subsume` tp_1)
      specify "trivial 2 rename" $
                          (tp_0 `Subsume` tp_1)
          `isEquivOf`     (tp_2 `Subsume` tp_3)

  describe "instance AlphaEquiv Constraint" $ do
      specify "trivial 1" $
                        C []
          `isEquivOf`   C []
      specify "trivial 2" $
                        C [TRUE]
          `isEquivOf`   C [TRUE]
      specify "trivial 3" $
                        C [tp_0 `Subsume` tp_1]
          `isEquivOf`   C [tp_0 `Subsume` tp_1]
      specify "trivial 4" $
                        Proj [] []
          `isEquivOf`   Proj [] []
      specify "trivial 5" $
                        Proj [] [TRUE]
          `isEquivOf`   Proj [] [TRUE]
      specify "trivial 6" $
                        Proj [] [tp_0 `Subsume` tp_1]
          `isEquivOf`   Proj [] [tp_0 `Subsume` tp_1]
      specify "duplicates" $
                        Proj [] [tp_0 `Subsume` tp_1, tp_0 `Subsume` tp_1]
          `isEquivOf`   Proj [] [tp_0 `Subsume` tp_1, tp_0 `Subsume` tp_1]

  describe "instance AlphaEquiv Type" $ do
    describe "type variables" $ do
        specify "trivial 1" $
                        tp_0 `isEquivOf` tp_0
        specify "trivial 2" $
                        tp_1 `isEquivOf` tp_0

    describe "functions" $ do
        specify "trivial" $
                            (tp_0 `Fun` tp_0)
            `isEquivOf`     (tp_0 `Fun` tp_0)
        specify "functions test  0a" $ 
                            (tp_1 `Fun` tp_1)
            `isEquivOf`     (tp_1 `Fun` tp_1)
        specify "functions test  1a" $ 
                            (tp_1 `Fun` tp_1)
            `isNotEquivOf`  (tp_1 `Fun` tp_0)
        specify "functions test  2a" $ 
                            (tp_1 `Fun` tp_1)
            `isNotEquivOf`  (tp_0 `Fun` tp_1)
        specify "functions test  3a" $ 
                            (tp_1 `Fun` tp_1)
            `isEquivOf`     (tp_0 `Fun` tp_0)
        specify "functions test  4a" $ 
                            (tp_1 `Fun` tp_0)
            `isNotEquivOf`  (tp_1 `Fun` tp_1)
        specify "functions test  5a" $ 
                            (tp_1 `Fun` tp_0)
            `isEquivOf`     (tp_1 `Fun` tp_0)
        specify "functions test  6a" $ 
                            (tp_1 `Fun` tp_0)
            `isEquivOf`     (tp_0 `Fun` tp_1)
        specify "functions test  7a" $ 
                            (tp_1 `Fun` tp_0)
            `isNotEquivOf`  (tp_0 `Fun` tp_0)
        specify "functions test  8a" $ 
                            (tp_0 `Fun` tp_1)
            `isNotEquivOf`  (tp_1 `Fun` tp_1)
        specify "functions test  9a" $ 
                            (tp_0 `Fun` tp_1)
            `isEquivOf`     (tp_1 `Fun` tp_0)
        specify "functions test 10a" $ 
                            (tp_0 `Fun` tp_1)
            `isEquivOf`     (tp_0 `Fun` tp_1)
        specify "functions test 11a" $ 
                            (tp_0 `Fun` tp_1)
            `isNotEquivOf`  (tp_0 `Fun` tp_0)
        specify "functions test 12a" $ 
                            (tp_0 `Fun` tp_0)
            `isEquivOf`     (tp_1 `Fun` tp_1)
        specify "functions test 13a" $ 
                            (tp_0 `Fun` tp_0)
            `isNotEquivOf`  (tp_1 `Fun` tp_0)
        specify "functions test 14a" $ 
                            (tp_0 `Fun` tp_0)
            `isNotEquivOf`  (tp_0 `Fun` tp_1)
        specify "functions test 15a" $ 
                            (tp_0 `Fun` tp_0)
            `isEquivOf`     (tp_0 `Fun` tp_0)
        specify "functions test  0b" $ 
                            (tp_8 `Fun` tp_8)
            `isEquivOf`     (tp_1 `Fun` tp_1)
        specify "functions test  1b" $ 
                            (tp_8 `Fun` tp_8)
            `isNotEquivOf`  (tp_1 `Fun` tp_0)
        specify "functions test  2b" $ 
                            (tp_8 `Fun` tp_8)
            `isNotEquivOf`  (tp_0 `Fun` tp_1)
        specify "functions test  3b" $ 
                            (tp_8 `Fun` tp_8)
            `isEquivOf`     (tp_0 `Fun` tp_0)
        specify "functions test  4b" $ 
                            (tp_8 `Fun` tp_9)
            `isNotEquivOf`  (tp_1 `Fun` tp_1)
        specify "functions test  5b" $ 
                            (tp_8 `Fun` tp_9)
            `isEquivOf`     (tp_1 `Fun` tp_0)
        specify "functions test  6b" $ 
                            (tp_8 `Fun` tp_9)
            `isEquivOf`     (tp_0 `Fun` tp_1)
        specify "functions test  7b" $ 
                            (tp_8 `Fun` tp_9)
            `isNotEquivOf`  (tp_0 `Fun` tp_0)
        specify "functions test  8b" $ 
                            (tp_9 `Fun` tp_8)
            `isNotEquivOf`  (tp_1 `Fun` tp_1)
        specify "functions test  9b" $ 
                            (tp_9 `Fun` tp_8)
            `isEquivOf`     (tp_1 `Fun` tp_0)
        specify "functions test 10b" $ 
                            (tp_9 `Fun` tp_8)
            `isEquivOf`     (tp_0 `Fun` tp_1)
        specify "functions test 11b" $ 
                            (tp_9 `Fun` tp_8)
            `isNotEquivOf`  (tp_0 `Fun` tp_0)
        specify "functions test 12b" $ 
                            (tp_9 `Fun` tp_9)
            `isEquivOf`     (tp_1 `Fun` tp_1)
        specify "functions test 13b" $ 
                            (tp_9 `Fun` tp_9)
            `isNotEquivOf`  (tp_1 `Fun` tp_0)
        specify "functions test 14b" $ 
                            (tp_9 `Fun` tp_9)
            `isNotEquivOf`  (tp_0 `Fun` tp_1)
        specify "functions test 15b" $ 
                            (tp_9 `Fun` tp_9)
            `isEquivOf`     (tp_0 `Fun` tp_0)

    describe "records" $ do
        specify "trivial" $
                            Record [("a",tp_0), ("b",tp_1), ("c",tp_2)]
            `isEquivOf`     Record [("a",tp_0), ("b",tp_1), ("c",tp_2)]
        specify "trivial reorder" $
                            Record [("a",tp_0), ("b",tp_1), ("c",tp_2)]
            `isEquivOf`     Record [("b",tp_1), ("a",tp_0), ("c",tp_2)]
        specify "trivial inverse" $
                            Record [("a",tp_0), ("b",tp_1), ("c",tp_2)]
            `isNotEquivOf`  Record [("x",tp_0), ("y",tp_1), ("z",tp_2)]
        specify "records test 1" $
                            Record [("a",tp_0), ("b",tp_1), ("c",tp_2)]
            `isEquivOf`     Record [("a",tp_7), ("b",tp_8), ("c",tp_9)]
        specify "records test 2" $
                            Record [("a",tp_0), ("b",tp_1), ("c",tp_2)]
            `isEquivOf`     Record [("a",tp_7), ("b",tp_1), ("c",tp_9)]
        specify "records test 3" $
                            Record [("a",tp_0), ("b",tp_1), ("c",tp_2)]
            `isNotEquivOf`  Record [("x",tp_7)]
        specify "records test 4" $
                            Record [("a",tp_0), ("b",tp_1), ("c",tp_2)]
            `isNotEquivOf`  Record []
        specify "records test 5" $
                            Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]
            `isEquivOf`     Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)]
        specify "records test 5 reorder" $
                            Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]
            `isEquivOf`     Record [("b",tp_8), ("a", tp_1 `Fun` tp_0), ("c",tp_9)]

    describe "records and functions" $ do
        specify "mixed records & functions 1" $
                            (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]))
            `isEquivOf`     (tp_1 `Fun` (Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)]))
        specify "mixed records & functions 1 mismatch" $
                            (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]))
            `isNotEquivOf`  (tp_1 `Fun` (Record [("x", tp_1 `Fun` tp_0), ("y",tp_8), ("z",tp_9)]))

        specify "mixed records & functions 2" $
                            (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]) `Fun` tp_9)
            `isEquivOf`     (tp_1 `Fun` (Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)]) `Fun` tp_0)
        specify "mixed records & functions 2 mismatch" $
                            (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]) `Fun` tp_9)
            `isNotEquivOf`  (tp_1 `Fun` (Record [("x", tp_1 `Fun` tp_0), ("y",tp_8), ("z",tp_9)]) `Fun` tp_0)
        specify "mixed records & functions 2 reorder" $
                            (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]) `Fun` tp_9)
            `isEquivOf`     (tp_1 `Fun` (Record [("c",tp_9), ("b",tp_8), ("a", tp_1 `Fun` tp_0)]) `Fun` tp_0)
        
        specify "mixed records & functions 3" $
                            (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_8)])
            `isNotEquivOf`  (Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)])
        specify "mixed records & functions 3 reorder" $
                            (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_8)])
            `isNotEquivOf`  (Record [("b",tp_8), ("c",tp_9), ("a", tp_1 `Fun` tp_0)])

        specify "mixed records & functions 4" $
                            (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_1)]))
            `isNotEquivOf`  (tp_1 `Fun` (Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)]))
        specify "mixed records & functions 4 reorder" $
                            (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_1)]))
            `isNotEquivOf`  (tp_1 `Fun` (Record [("c",tp_9), ("a", tp_1 `Fun` tp_0), ("b",tp_8)]))

        specify "mixed records & functions 5" $
                            (tp_8 `Fun` (Record [("a", tp_1 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]) `Fun` tp_9)
            `isNotEquivOf`  (tp_1 `Fun` (Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)]) `Fun` tp_0)
        specify "mixed records & functions 5 reorder" $
                            (tp_8 `Fun` (Record [("a", tp_1 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]) `Fun` tp_9)
            `isNotEquivOf`  (tp_1 `Fun` (Record [("b",tp_8), ("a", tp_1 `Fun` tp_0), ("a",tp_9)]) `Fun` tp_0)
        specify "mixed records & functions 6" $
                            (tp_0 `Fun` (Record [("foo", tp_0 `Fun` tp_1)]) `Fun` tp_1)
            `isEquivOf`     (tp_0 `Fun` (Record [("foo", tp_0 `Fun` tp_1)]) `Fun` tp_1)

  describe "instance AlphaEquiv TypeScheme" $ do
    describe "type schemes" $ do
      describe "Mono" $ do

          specify "Mono version of: mixed records & functions 1" $
                              Mono (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]))
              `isEquivOf`     Mono (tp_1 `Fun` (Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)]))
          specify "Mono version of: mixed records & functions 1 mismatch" $
                              Mono (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]))
              `isNotEquivOf`  Mono (tp_1 `Fun` (Record [("x", tp_1 `Fun` tp_0), ("y",tp_8), ("z",tp_9)]))

          specify "Mono version of: mixed records & functions 2" $
                              Mono (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]) `Fun` tp_9)
              `isEquivOf`     Mono (tp_1 `Fun` (Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)]) `Fun` tp_0)
          specify "Mono version of: mixed records & functions 2 mismatch" $
                              Mono (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]) `Fun` tp_9)
              `isNotEquivOf`  Mono (tp_1 `Fun` (Record [("x", tp_1 `Fun` tp_0), ("y",tp_8), ("z",tp_9)]) `Fun` tp_0)
          specify "Mono version of: mixed records & functions 2 reorder" $
                              Mono (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]) `Fun` tp_9)
              `isEquivOf`     Mono (tp_1 `Fun` (Record [("c",tp_9), ("b",tp_8), ("a", tp_1 `Fun` tp_0)]) `Fun` tp_0)
          
          specify "Mono version of: mixed records & functions 3" $
                              Mono (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_8)])
              `isNotEquivOf`  Mono (Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)])
          specify "Mono version of: mixed records & functions 3 reorder" $
                              Mono (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_8)])
              `isNotEquivOf`  Mono (Record [("b",tp_8), ("c",tp_9), ("a", tp_1 `Fun` tp_0)])

          specify "Mono version of: mixed records & functions 4" $
                              Mono (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_1)]))
              `isNotEquivOf`  Mono (tp_1 `Fun` (Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)]))
          specify "Mono version of: mixed records & functions 4 reorder" $
                              Mono (tp_8 `Fun` (Record [("a", tp_8 `Fun` tp_9), ("b",tp_1), ("c",tp_1)]))
              `isNotEquivOf`  Mono (tp_1 `Fun` (Record [("c",tp_9), ("a", tp_1 `Fun` tp_0), ("b",tp_8)]))

          specify "Mono version of: mixed records & functions 5" $
                              Mono (tp_8 `Fun` (Record [("a", tp_1 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]) `Fun` tp_9)
              `isNotEquivOf`  Mono (tp_1 `Fun` (Record [("a", tp_1 `Fun` tp_0), ("b",tp_8), ("c",tp_9)]) `Fun` tp_0)
          specify "Mono version of: mixed records & functions 5 reorder" $
                              Mono (tp_8 `Fun` (Record [("a", tp_1 `Fun` tp_9), ("b",tp_1), ("c",tp_2)]) `Fun` tp_9)
              `isNotEquivOf`  Mono (tp_1 `Fun` (Record [("b",tp_8), ("a", tp_1 `Fun` tp_0), ("a",tp_9)]) `Fun` tp_0)
          specify "Mono version of: mixed records & functions 6" $
                              Mono (tp_0 `Fun` (Record [("foo", tp_0 `Fun` tp_1)]) `Fun` tp_1)
              `isEquivOf`     Mono (tp_0 `Fun` (Record [("foo", tp_0 `Fun` tp_1)]) `Fun` tp_1)

      describe "Poly" $ do
          describe "no projection" $ do
              describe "`id`-based" $ do
                  specify "trivial" $
                                      Poly [tv_0] (C [TRUE]) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_0] (C [TRUE]) (tp_0 `Fun` tp_0)
                  specify "trivial negative" $
                                      Poly [tv_0]       (C [TRUE]) (tp_0 `Fun` tp_0)
                      `isNotEquivOf`  Poly [tv_0, tv_1] (C [TRUE]) (tp_0 `Fun` tp_1)
                  specify "rename" $
                                      Poly [tv_0] (C [TRUE]) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (C [TRUE]) (tp_1 `Fun` tp_1)
                  specify "rename inv" $
                                      Poly [tv_0]      (C [TRUE]) (tp_0 `Fun` tp_0)
                      `isNotEquivOf`  Poly [tv_1,tv_2] (C [TRUE]) (tp_1 `Fun` tp_2)
                  specify "empty constraints" $
                                      Poly [tv_0] (C []) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (C []) (tp_1 `Fun` tp_1)
                  specify "constraints redundancy" $
                                      Poly [tv_0] (C [TRUE]) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (C [TRUE, TRUE]) (tp_1 `Fun` tp_1)
                  specify "constraints redundancy (TRUE is an identity element)" $
                                      Poly [tv_0] (C []) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (C [TRUE, TRUE]) (tp_1 `Fun` tp_1)
                  specify "constraints redundancy ( ∵  ⍺≼⍺ ≡ TRUE  ∴  ⍺≼⍺ is an identity element)" $
                                      Poly [tv_0] (C []) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (C [TRUE, tp_1 `Subsume` tp_1]) (tp_1 `Fun` tp_1)
                  specify "constraints redundancy ( ∵  ⍺≼⍺ ≡ TRUE  ∴  ⍺≼⍺ is an identity element)" $
                                      Poly [tv_0] (C []) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (C [TRUE, tp_1 `Subsume` tp_1]) (tp_1 `Fun` tp_1)

                  specify "∀-quant. reorder" $
                                      Poly [tv_1, tv_0] (C [tp_0 `Subsume` tp_1]) (tp_1 `Fun` tp_1)
                      `isEquivOf`     Poly [tv_2, tv_3] (C [tp_2 `Subsume` tp_3]) (tp_3 `Fun` tp_3)

                  specify "∀-quant. unused" $
                                      Poly [tv_1, tv_0]             (C [tp_0 `Subsume` tp_1]) (tp_1 `Fun` tp_1)
                      `isEquivOf`     Poly [tv_2, tv_3, tv_4, tv_5] (C [tp_2 `Subsume` tp_3]) (tp_3 `Fun` tp_3)

                  specify "constraints mismatch" $
                                      Poly [tv_0, tv_1] (C [TRUE])                (tp_1 `Fun` tp_1)
                      `isNotEquivOf`  Poly [tv_2, tv_3] (C [tp_2 `Subsume` tp_3]) (tp_3 `Fun` tp_3)
                  specify "constraints mismatch (∀-quant. reorder)" $
                                      Poly [tv_1, tv_0] (C [TRUE])                (tp_1 `Fun` tp_1)
                      `isNotEquivOf`  Poly [tv_2, tv_3] (C [tp_2 `Subsume` tp_3]) (tp_3 `Fun` tp_3)

          describe "projection: equivalent to simple constraints (empty tvar list)" $ do
              describe "`id`-based" $ do
                  specify "trivial" $
                                      Poly [tv_0] (Proj [] [TRUE]) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_0] (Proj [] [TRUE]) (tp_0 `Fun` tp_0)
                  specify "rename" $
                                      Poly [tv_0] (Proj [] [TRUE]) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (Proj [] [TRUE]) (tp_1 `Fun` tp_1)
                  specify "empty constraints" $
                                      Poly [tv_0] (Proj [] []) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (Proj [] []) (tp_1 `Fun` tp_1)
                  specify "constraints redundancy" $
                                      Poly [tv_0] (Proj [] [TRUE]) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (Proj [] [TRUE, TRUE]) (tp_1 `Fun` tp_1)
                  specify "constraints redundancy (TRUE is an identity element)" $
                                      Poly [tv_0] (Proj [] []) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (Proj [] [TRUE, TRUE]) (tp_1 `Fun` tp_1)
                  specify "constraints redundancy ( ∵  ⍺≼⍺ ≡ TRUE  ∴  ⍺≼⍺ is an identity element)" $
                                      Poly [tv_0] (Proj [] []) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (Proj [] [TRUE, tp_1 `Subsume` tp_1]) (tp_1 `Fun` tp_1)
                  specify "constraints redundancy ( ∵  ⍺≼⍺ ≡ TRUE  ∴  ⍺≼⍺ is an identity element)" $
                                      Poly [tv_0] (Proj [] []) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_1] (Proj [] [TRUE, tp_1 `Subsume` tp_1]) (tp_1 `Fun` tp_1)

          describe "projection: introducing tvars" $ do
              specify "trivial 1" $
                                  Poly [tv_0, tv_1] (Proj [tv_3] [TRUE, TRUE]) tp_4
                  `isEquivOf`     Poly [tv_0, tv_1] (Proj [tv_3] [TRUE, TRUE]) tp_4
              specify "trivial 1 redundancy" $
                                  Poly [tv_0, tv_1]             (Proj [tv_3] [TRUE, TRUE]) tp_4
                  `isEquivOf`     Poly [tv_0, tv_1, tv_0, tv_0] (Proj [tv_3] [TRUE, TRUE]) tp_4

              describe "`id`-based" $ do
                  specify "trivial" $
                                      Poly [tv_0] (Proj [tv_1] [TRUE]) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_0] (Proj [tv_1] [TRUE]) (tp_0 `Fun` tp_0)
                  specify "rename" $
                                      Poly [tv_0] (Proj [tv_1] [TRUE]) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_2] (Proj [tv_3] [TRUE]) (tp_2 `Fun` tp_2)

                  specify "constraints redundancy" $
                                      Poly [tv_0] (Proj [tv_1] []) (tp_0 `Fun` tp_0)
                      `isEquivOf`     Poly [tv_2] (Proj [tv_3] [TRUE, tp_2 `Subsume` tp_2, tp_3 `Subsume` tp_3]) (tp_2 `Fun` tp_2)

              specify "mix 1" $
                                  Poly [tv_0, tv_1, tv_2, tv_3] (Proj []     [tp_1 `Subsume` tp_2]) (tp_1 `Fun` tp_2)
                  `isNotEquivOf`  Poly [tv_4, tv_5, tv_6]       (Proj [tv_7] [tp_7 `Subsume` tp_5]) (tp_7 `Fun` tp_5)

              specify "mix 2" $
                                  Poly [tv_0, tv_1, tv_2, tv_3] (Proj [] [tp_1 `Subsume` tp_2]) (tp_1 `Fun` tp_2)
                  `isNotEquivOf`  Poly [tv_4, tv_5, tv_6]       (Proj [] [tp_7 `Subsume` tp_5]) (tp_7 `Fun` tp_5)

              specify "mix 1a" $
                                  Poly [tv_0, tv_1, tv_2, tv_3]       (Proj [] [tp_1 `Subsume` tp_2]) (tp_1 `Fun` tp_2)
                  `isEquivOf`     Poly [tv_4, tv_7, tv_5, tv_6, tv_9] (Proj [] [tp_7 `Subsume` tp_5]) (tp_7 `Fun` tp_5)

              specify "mix 1a constraints reorder" $
                                  Poly [tv_0, tv_1, tv_2, tv_3] (Proj [] [tp_1 `Subsume` tp_2, tp_2 `Subsume` tp_1]) (tp_1 `Fun` tp_2)
                  `isEquivOf`     Poly [tv_4, tv_7, tv_5, tv_6] (Proj [] [tp_5 `Subsume` tp_7, tp_7 `Subsume` tp_5]) (tp_7 `Fun` tp_5)

          specify "`const`" $
                              Poly [tv_0, tv_1] (C [TRUE]) (tp_0 `Fun` tp_1 `Fun` tp_0)
              `isEquivOf`     Poly [tv_2, tv_3] (C [TRUE]) (tp_2 `Fun` tp_3 `Fun` tp_2)

  where
    tp_0 = TV tv_0
    tp_1 = TV tv_1
    tp_2 = TV tv_2
    tp_3 = TV tv_3
    tp_4 = TV tv_4
    tp_5 = TV tv_5
    tp_6 = TV tv_6
    tp_7 = TV tv_7
    tp_8 = TV tv_8
    tp_9 = TV tv_9
    tv_0 = TVar 0
    tv_1 = TVar 1
    tv_2 = TVar 2
    tv_3 = TVar 3
    tv_4 = TVar 4
    tv_5 = TVar 5
    tv_6 = TVar 6
    tv_7 = TVar 7
    tv_8 = TVar 8
    tv_9 = TVar 9

