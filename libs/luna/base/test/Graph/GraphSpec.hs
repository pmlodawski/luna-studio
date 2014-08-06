---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Graph.GraphSpec where

import Test.Hspec

import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                     as Crumb
import           Flowbox.Luna.Data.AST.Expr                            (Expr)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                    as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper                   as Zipper
import qualified Flowbox.Luna.Data.Graph.Edge                          as Edge
import           Flowbox.Luna.Data.Graph.Graph                         (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                         as Graph
import qualified Flowbox.Luna.Data.Graph.Node                          as Node
import qualified Flowbox.Luna.Data.Graph.Port                          as Port
import           Flowbox.Luna.Data.Pass.AliasInfo                      (AliasInfo)
import           Flowbox.Luna.Data.Pass.Source                         (Source (Source))
import           Flowbox.Luna.Data.PropertyMap                         (PropertyMap)
import qualified Flowbox.Luna.Passes.Analysis.Alias.Alias              as Alias
import           Flowbox.Luna.Passes.Transform.AST.IDFixer.IDFixer     (clearIDs)
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser as TxtParser
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder   as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser     as GraphParser
import           Flowbox.Prelude
import           Graph.SampleCodes                                     (sampleCodes)



getAST :: String -> IO (Expr, AliasInfo)
getAST code = eitherStringToM' $ runEitherT $ do
    (ast, _, _) <- EitherT $ TxtParser.run $ Source ["Main"] code
    let bc = [Crumb.Module "Main", Crumb.Function "main" []]
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast
    aa    <- EitherT $ Alias.run ast
    expr  <- Focus.getFunction focus <??> "Target is not a function"
    return (expr, aa)


getGraph :: AliasInfo -> PropertyMap -> Expr -> IO (Graph, PropertyMap)
getGraph = eitherStringToM' .:. GraphBuilder.run


getExpr :: Graph -> PropertyMap -> Expr -> IO Expr
getExpr = eitherStringToM' .:. GraphParser.run


backAndForth :: String -> IO ()
backAndForth code = do
    (expr, aa)    <- getAST code
    (graph, pm)   <- getGraph aa def expr
    --printLn
    --print expr
    --print graph
    --print pm
    --printLn
    expr2         <- getExpr graph pm expr
    (graph2, pm2) <- getGraph aa def expr
    (clearIDs 0 expr2) `shouldBe` (clearIDs 0 expr)
    graph2 `shouldBe` graph
    pm2    `shouldBe` pm


named :: a -> b -> (a, b)
named = (,)


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "ast <-> graph conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth code) sampleCodes

    describe "graph sort algoritm" $ do
        it "sorts graph correctly" $ do
            let n1 = (1, Node.Expr "" "" 1 0)
                n2 = (2, Node.Expr "" "" 2 0)
                n3 = (3, Node.Expr "" "" 2 0)
                n4 = (4, Node.Expr "" "" 3 0)
                n5 = (5, Node.Expr "" "" 4 0)
                n6 = (6, Node.Expr "" "" 5 0)
                properOrder = [n1, n2, n4, n5, n3, n6]
                testOrder   = [n2, n3, n5, n6, n4, n1]
                edges  = [(1, 2, Edge.Data Port.All 0)
                         ,(5, 3, Edge.Data Port.All 0)]
                graph  = Graph.mkGraph testOrder edges
                sorted = Graph.sort graph
            map fst sorted `shouldBe` map fst properOrder
            sorted         `shouldBe` properOrder
