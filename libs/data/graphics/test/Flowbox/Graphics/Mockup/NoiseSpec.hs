module Flowbox.Graphics.Mockup.NoiseSpec where

import qualified Flowbox.Graphics.Image.Channel as Chan
import qualified Flowbox.Graphics.Image.Image   as Img
import qualified Flowbox.Graphics.Image.View    as View
import           Flowbox.Graphics.Mockup.Basic  as M
import           Flowbox.Graphics.Mockup.Noise  as M
import           Flowbox.Graphics.Utils.Utils   (clamp')
import           Test.Hspec
import           Test.QuickCheck

import           Flowbox.Prelude                as P

import           TestHelpers

spec :: Spec
spec = do
    let specPath = "./test/Flowbox/Graphics/Mockup/"
        in do
            let testName = "noiseLuna"

            let actualImage = (Img.map (View.map (Chan.unsafeMap (Chan.FunFloat (clamp' 0 1))))) $ noiseLuna (Perlin Standard 33.3 2.5 10 0.7 100 0.5) 700 400
            describe testName $ do
                it "should save img" $ do
                    pending
                    testSave actualImage `shouldReturn` ()

            defaultReferenceTest testName specPath actualImage
