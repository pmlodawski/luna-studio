module Test.Graph.MarkerSpec (spec) where

import Empire.Prelude

import qualified Empire.Commands.Graph         as Graph
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import qualified LunaStudio.Data.TextDiff      as TextDiff

import LunaStudio.Data.TextDiff (mkTextDiff)
import Test.Hspec               (Spec, it)
import Test.Hspec.Empire        (runTests, testCaseWithMarkers)
import Text.RawString.QQ        (r)


spec :: Spec
spec = runTests "code markers tests" $ do
    it "handles markers correctly after fixing incorrect code" $ let
        initialCode = [r|
            import Std.Base

            «0»def main:
                «1»N
                «2»one
            |]
        expectedCode = [r|
            import Std.Base

            «0»def main:
                None
            |]
        in testCaseWithMarkers initialCode expectedCode $ \gl -> do
            Graph.substituteCode
                (gl ^. GraphLocation.filePath)
                [mkTextDiff (5,3) (4,4) "" def]
