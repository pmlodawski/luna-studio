---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Pass.Transform.Graph.GraphViewSpec where

import Control.Monad (forM_)
import Test.Hspec

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.DEP.AST.Control.Crumb                (Breadcrumbs)
import           Luna.Graph.PropertyMap                (PropertyMap)
import           Luna.Graph.View.GraphView             (GraphView)
import qualified Luna.Graph.View.GraphView             as GraphView
import qualified Test.Luna.DEP.AST.Common                  as Common
import qualified Test.Luna.Pass.Transform.Graph.Common as Common
import           Test.Luna.Sample.Code                 (sampleCodes)
import           Test.Luna.Sample.GraphView            (sampleGraphs)



backAndForth :: Breadcrumbs -> String -> IO ()
backAndForth bc code = do
    expr          <- Common.getAST code
    (graph , pm)  <- Common.getGraph bc def expr
    let (graphview, pm2) = GraphView.fromGraph graph pm
    (graph3, pm3) <- eitherStringToM $ GraphView.toGraph graphview pm2

    graph `shouldBe` graph3
    pm    `shouldBe` pm3


backAndForth2 :: GraphView -> PropertyMap -> IO ()
backAndForth2 graphview initPM = do
    --printLn
    --print graphview
    (graph, pm) <- eitherStringToM $ GraphView.toGraph graphview initPM
    --printLn
    --print graph
    --printLn
    let (graphview2, pm2) = GraphView.fromGraph graph pm

    graphview2 `shouldBe` graphview
    pm2        `shouldBe` initPM


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "code -> graph <-> graphview conversion" $ do
        forM_ sampleCodes $ \(name, code) ->
            it ("returns the same when converting back and forth - " ++ name) $
                backAndForth Common.mainBC code

    describe "graphview <-> graph conversion" $ do
        forM_ sampleGraphs $ \(name, gv) ->
            it ("returns the same when converting back and forth - " ++ name) $
                backAndForth2 gv def
