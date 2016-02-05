module Flowbox.Graphics.Shader.ShaderSpec where

import           Flowbox.Graphics.Shader.Shader as S
import           Test.Hspec
import           Test.QuickCheck

import           Flowbox.Prelude                as P


spec :: Spec
spec = do
    describe "runShader" $ do
        it "on id unit shader should return its argument" $ do
            property $ \x -> runShader (unitShader id) x `shouldBe` (x ::Int)
