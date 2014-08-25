---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Flowbox.System.UniPathSpec where

import Test.Hspec

import           Flowbox.Prelude
import qualified Flowbox.System.UniPath as UniPath



main :: IO ()
main = hspec spec



backAndForth path = 
    UniPath.toUnixString (UniPath.fromUnixString path) `shouldBe` path


spec :: Spec
spec = do
    describe "UniPath" $ do
        it "returns the same when converting back and forth" $ do
            backAndForth "."
