---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Pass.Transform.Graph.GraphViewSpec where

import           Test.Hspec

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.Control.Crumb                    (Breadcrumbs)
import           Luna.Syntax.Graph.Tag                 (Tag)
import qualified Luna.Syntax.Graph.Tag                 as Tag
import           Luna.Syntax.Graph.View.GraphView      (GraphView)
import qualified Luna.Syntax.Graph.View.GraphView      as GraphView
import qualified Luna.Util.Label                       as Label
import qualified Test.Luna.Pass.Transform.Graph.Common as Common
import           Test.Luna.Sample.Code                 (sampleCodes)
import           Test.Luna.Sample.GraphView            (sampleGraphs)
import qualified Test.Luna.Syntax.AST                  as Common



backAndForth :: Breadcrumbs -> String -> IO ()
backAndForth bc code = do
    (ast', _astInfo) <- Common.getAST code
    let ast = Label.replace Tag.fromEnumerated ast'
    (_ast2, graph)  <- Common.getGraph bc ast
    let graphview = GraphView.fromGraph graph
    graph3 <- eitherStringToM $ GraphView.toGraph graphview
    graph `shouldBe` graph3


backAndForth2 :: GraphView Tag () -> IO ()
backAndForth2 graphview = do
    --printLn
    --print graphview
    graph <- eitherStringToM $ GraphView.toGraph graphview
    --printLn
    --print graph
    --printLn
    let graphview2 = GraphView.fromGraph graph
    graphview2 `shouldBe` graphview


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "code -> graph <-> graphview conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth Common.mainBC code) sampleCodes

    describe "graphview <-> graph conversion" $ do
        mapM_ (\(name, gv) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth2 gv) sampleGraphs
