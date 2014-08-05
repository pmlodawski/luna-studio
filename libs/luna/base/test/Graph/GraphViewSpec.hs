---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Graph.GraphViewSpec where

import Test.Hspec

import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                     as Crumb
import           Flowbox.Luna.Data.AST.Expr                            (Expr)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                    as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper                   as Zipper
import           Flowbox.Luna.Data.Graph.Graph                         (Graph)
import qualified Flowbox.Luna.Data.GraphView.GraphView                 as GraphView
import           Flowbox.Luna.Data.Pass.AliasInfo                      (AliasInfo)
import           Flowbox.Luna.Data.Pass.Source                         (Source (Source))
import           Flowbox.Luna.Data.PropertyMap                         (PropertyMap)
import qualified Flowbox.Luna.Passes.Analysis.Alias.Alias              as Alias
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
    (expr, aa)  <- getAST code
    (graph, pm) <- getGraph aa def expr

    let graphview   = GraphView.fromGraph graph
        graphWithPm = GraphView.toGraph graphview pm

    Right (graph, pm) `shouldBe` graphWithPm


named :: a -> b -> (a, b)
named = (,)


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "graph <-> graphview conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth code) sampleCodes

