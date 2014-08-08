---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Graph.GraphSpec where

import Test.Hspec

import qualified Flowbox.Luna.Data.Graph.Edge                      as Edge
import qualified Flowbox.Luna.Data.Graph.Graph                     as Graph
import qualified Flowbox.Luna.Data.Graph.Node                      as Node
import qualified Flowbox.Luna.Data.Graph.Port                      as Port
import           Flowbox.Luna.Passes.Transform.AST.IDFixer.IDFixer (clearIDs)
import           Flowbox.Prelude
import qualified Graph.Common                                      as Common
import           Graph.SampleCodes                                 (sampleCodes)



backAndForth :: String -> IO ()
backAndForth code = do
    (expr, aa)    <- Common.getAST code
    (graph, pm)   <- Common.getGraph aa def expr
    --printLn
    --print expr
    --printLn
    --print graph
    --print pm
    --printLn
    expr2         <- Common.getExpr graph pm expr
    (graph2, pm2) <- Common.getGraph aa def expr
    (clearIDs 0 expr2) `shouldBe` (clearIDs 0 expr)
    graph2 `shouldBe` graph
    pm2    `shouldBe` pm


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "ast <-> graph conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth code) sampleCodes

    describe "graph sort alghorithm" $ do
        it "sorts graph correctly" $ do
            let n1 = (1, Node.Expr "" "" (1, 0))
                n2 = (2, Node.Expr "" "" (2, 0))
                n3 = (3, Node.Expr "" "" (2, 0))
                n4 = (4, Node.Expr "" "" (3, 0))
                n5 = (5, Node.Expr "" "" (4, 0))
                n6 = (6, Node.Expr "" "" (5, 0))
                properOrder = [n1, n2, n4, n5, n3, n6]
                testOrder   = [n2, n3, n5, n6, n4, n1]
                edges  = [(1, 2, Edge.Data Port.All 0)
                         ,(5, 3, Edge.Data Port.All 0)]
                graph  = Graph.mkGraph testOrder edges
                sorted = Graph.sort graph
            map fst sorted `shouldBe` map fst properOrder
            sorted         `shouldBe` properOrder
