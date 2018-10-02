module Test.Typecheck.PatternMatchSpec (spec) where

import Empire.Prelude

import qualified LunaStudio.Data.Node as Node

import LunaStudio.Data.LabeledTree    (LabeledTree (LabeledTree))
import LunaStudio.Data.Port           (OutPortIndex (Projection),
                                       OutPorts (OutPorts), Port (Port),
                                       PortState (NotConnected))
import LunaStudio.Data.TypeRep        (TypeRep (TStar))
import Test.Hspec                     (Spec, describe, it)
import Test.Hspec.Empire              (findNodeByName,
                                       mkAllPort, noAction, runTests, testCaseWithTC)
import Test.Hspec.Expectations.Lifted (shouldBe)
import Text.RawString.QQ              (r)


spec :: Spec
spec = runTests "typechecker pattern match tests" $ do
    describe "ports tests" $ do
        it "contains proper ports for pattern match on custom class" $ let
            code = [r|
                import Std.Base

                class Vector:
                    x y z :: Int

                def main:
                    v = Vector 1 2 3
                    Vector a b c = v
                    None
                |]
            prepare gl = do
                Just patternMatch <- findNodeByName gl "Vector a b c"
                pure patternMatch
            expectedPatternMatchOutPorts = LabeledTree
                (OutPorts
                    [ LabeledTree def $ Port [Projection 0] "a" TStar NotConnected
                    , LabeledTree def $ Port [Projection 1] "b" TStar NotConnected
                    , LabeledTree def $ Port [Projection 2] "c" TStar NotConnected ])
                (mkAllPort "Vector a b c" NotConnected)
            in testCaseWithTC code code noAction $ \gl -> do
                patternMatch <- prepare gl
                let patternMatchOutPorts = patternMatch ^. Node.outPorts
                patternMatchOutPorts `shouldBe` expectedPatternMatchOutPorts
