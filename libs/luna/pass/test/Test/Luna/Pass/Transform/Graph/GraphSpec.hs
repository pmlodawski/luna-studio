---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Pass.Transform.Graph.GraphSpec where

import           Test.Hspec

import           Flowbox.Prelude
import           Luna.Control.Crumb                    (Breadcrumbs)
import qualified Luna.Syntax.Graph.Edge                as Edge
import           Luna.Syntax.Graph.Graph               (Graph)
import qualified Luna.Syntax.Graph.Graph               as Graph
import qualified Luna.Syntax.Graph.Node                as Node
import qualified Luna.Syntax.Graph.Port                as Port
import           Luna.Syntax.Graph.Tag                 (Tag)
import qualified Luna.Syntax.Graph.Tag                 as Tag
import qualified Luna.Util.Label                       as Label
import qualified Test.Luna.Pass.Transform.Graph.Common as Common
import           Test.Luna.Sample.Code                 (sampleCodes)
import qualified Test.Luna.Sample.Code                 as SampleCode
import           Test.Luna.Sample.Graph                (V, strExpr)
import qualified Test.Luna.Sample.Graph                as Graph
import qualified Test.Luna.Syntax.AST                  as AST



printHeader :: MonadIO m => String -> m ()
printHeader header = printLn >> putStrLn ("=== " <> header <> " ===") >> printLn

cleanIDs :: Tag -> Tag
cleanIDs = Tag.idTag .~ def

cleanTags :: Tag -> Tag
cleanTags = Tag.fromEnumerated

cleanFolded :: Tag -> Tag
cleanFolded = (Tag.folded .~ False) . cleanIDs


backAndForth :: Breadcrumbs -> String -> IO ()
backAndForth bc code = do
    --printLn >> printLn >> printLn
    (ast', astInfo) <- AST.getAST code
    let ast = Label.replace Tag.fromEnumerated ast'
    --prettyPrint ast
    --printHeader "getGraph"
    (ast2, graph2) <- Common.getGraph bc ast
    --prettyPrint graph2
    --printHeader "getExpr"
    (ast3, _astInfo3) <- Common.getExpr bc graph2 ast2 astInfo
    --prettyPrint ast3
    --putStrLn "getGraph"
    (ast4, graph4) <- Common.getGraph bc ast3
    --prettyPrint graph4
    expr  <- Common.getMain ast
    expr2 <- Common.getMain ast2
    expr4 <- Common.getMain ast4
    Label.replaceDecl  cleanTags   expr2  `shouldBe` Label.replaceDecl  cleanTags   expr
    Label.replaceGraph cleanFolded graph4 `shouldBe` Label.replaceGraph cleanFolded graph2
    Label.replaceDecl  cleanFolded expr4  `shouldBe` Label.replaceDecl  cleanFolded expr2


backAndForth2 :: Breadcrumbs -> Graph Tag V -> IO ()
backAndForth2 bc providedGraph = do
    (ast',  astInfo)  <- AST.getAST SampleCode.emptyMain
    let ast = Label.replace Tag.fromEnumerated ast'
    (ast2, _astInfo2) <- Common.getExpr bc providedGraph ast astInfo
    --printLn
    --prettyPrint ast2
    --printLn
    (ast3, resultGraph) <- Common.getGraph bc ast2
    --prettyPrint resultGraph
    --printLn
    --prettyPrint ast3
    --printLn
    Graph.toStringNodes resultGraph `shouldBe` providedGraph

backAndForth2' :: Breadcrumbs -> Graph Tag V -> Graph Tag V -> IO ()
backAndForth2' bc providedGraph expectedGraph = do
    (ast',  astInfo)  <- AST.getAST SampleCode.emptyMain
    let ast = Label.replace Tag.fromEnumerated ast'
    (ast2, _astInfo2) <- Common.getExpr bc providedGraph ast astInfo
    --printLn
    --prettyPrint ast2
    --printLn
    (ast3, resultGraph) <- Common.getGraph bc ast2
    --prettyPrint ast3
    --printLn
    resultGraph `shouldBe` expectedGraph


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "ast <-> graph conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth Common.mainBC code) sampleCodes
        --mapM_ (\(name, bc, code) -> it ("returns the same when converting back and forth - " ++ name) $
        --        backAndForth bc code) SampleCode.sampleLambdas

    describe "graph <-> ast conversion" $ do
        mapM_ (\(name, graph) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth2 Common.mainBC graph) Graph.samples
        mapM_ (\(name, providedGraph, expectedGraph) -> it ("fixes buggy graphs - " ++ name) $
                backAndForth2' Common.mainBC providedGraph expectedGraph) Graph.buggy


    describe "graph sort alghorithm" $ do
        it "sorts graph correctly" $ do
            let n1 = (1, Node.Expr (strExpr "") def def (1, 0) def False)
                n2 = (2, Node.Expr (strExpr "") def def (2, 0) def False)
                n3 = (3, Node.Expr (strExpr "") def def (2, 0) def False)
                n4 = (4, Node.Expr (strExpr "") def def (3, 0) def False)
                n5 = (5, Node.Expr (strExpr "") def def (4, 0) def False)
                n6 = (6, Node.Expr (strExpr "") def def (5, 0) def False)
                properOrder = [n1, n2, n4, n5, n3, n6]
                testOrder   = [n2, n3, n5, n6, n4, n1]
                edges  = [(1, 2, Edge.Data Port.mkSrcAll $ Port.mkDst 0)
                         ,(5, 3, Edge.Data Port.mkSrcAll $ Port.mkDst 0)]
                graph  = Graph.mkGraph testOrder edges
                sorted = Graph.sort graph
            map fst sorted `shouldBe` map fst properOrder
            sorted         `shouldBe` properOrder
