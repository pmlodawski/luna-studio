---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Flowbox.System.UniPathSpec where

import Test.Hspec

import           Flowbox.Prelude
import           Flowbox.System.UniPath (UniPath)
import qualified Flowbox.System.UniPath as UniPath



main :: IO ()
main = hspec spec


fromString :: FilePath -> UniPath
fromString = UniPath.fromUnixString

toString :: UniPath -> FilePath
toString = UniPath.toUnixString

sameBackAndForth :: String -> Expectation
sameBackAndForth path =
    toString (fromString path) `shouldBe` path


spec :: Spec
spec = do
    describe "UniPath" $ do
        it "returns the same when converting back and forth" $ do
            sameBackAndForth "."
            sameBackAndForth ".."
            sameBackAndForth "../.."
            sameBackAndForth "../../."
            sameBackAndForth "foo"
            sameBackAndForth "foo/"
            sameBackAndForth "foo/bar"
            sameBackAndForth "foo/bar/"
            sameBackAndForth "foo/bar/baz"
            sameBackAndForth "foo/bar/baz/"
            sameBackAndForth "/"
            sameBackAndForth "/foo"
            sameBackAndForth "/foo/"
            sameBackAndForth "/foo/bar"
            sameBackAndForth "/foo/bar/"
            sameBackAndForth "/foo/bar/baz"
            sameBackAndForth "/foo/bar/baz/"
            sameBackAndForth "C:/foo"
            sameBackAndForth "C:/foo/"
            sameBackAndForth "C:/foo/bar"
            sameBackAndForth "C:/foo/bar/"
            sameBackAndForth "C:/foo/bar/baz"
            sameBackAndForth "C:/foo/bar/baz/"
            sameBackAndForth "C:/foo/bar/baz/.."

        it "normalises path" $ do
            toString (UniPath.normalise $ fromString "./foo/bar"   ) `shouldBe` "./foo/bar"
            toString (UniPath.normalise $ fromString "./foo/.."    ) `shouldBe` "."
            toString (UniPath.normalise $ fromString "./foo/../bar") `shouldBe` "./bar"
            toString (UniPath.normalise $ fromString "/foo/.."     ) `shouldBe` "/"
            toString (UniPath.normalise $ fromString "/foo/../.."  ) `shouldBe` "/"

        it "computes base path" $ do
            toString (UniPath.basePath $ fromString "./foo/bar") `shouldBe` "./foo"
