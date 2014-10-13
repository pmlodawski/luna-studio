module Test.Luna.TypecheckerSpec (spec) where


import Luna.Typechecker.AST

import Test.Hspec


spec :: Spec
spec = do
  
  describe "trivia" $

    describe "compilation" $
      it "works" True
    -- compilation

  -- trivia

  describe "basic AST" $
    it "exists" True
  -- basic AST
