---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Graph.GraphViewSpec where

import Test.Hspec

import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.GraphView.GraphView as GraphView
import           Flowbox.Prelude
import qualified Graph.Common                          as Common
import           Graph.SampleCodes                     (sampleCodes)



backAndForth :: String -> IO ()
backAndForth code = do
    (expr, aa)  <- Common.getAST code
    (graph, pm) <- Common.getGraph aa def expr
    let graphview = GraphView.fromGraph graph
    (graph2, pm2) <- eitherStringToM $ GraphView.toGraph graphview pm

    graph `shouldBe` graph2
    pm    `shouldBe` pm2


main :: IO ()
main = hspec spec


spec :: Spec
spec =
    describe "graph <-> graphview conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth code) sampleCodes


