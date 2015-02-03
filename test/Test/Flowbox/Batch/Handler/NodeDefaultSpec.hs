---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}

module Test.Flowbox.Batch.Handler.NodeDefaultSpec where

import Data.List  ((\\))
import Test.Hspec

import qualified Flowbox.Batch.Batch                           as Batch
import qualified Flowbox.Batch.Handler.AST                     as Batch
import qualified Flowbox.Batch.Handler.Common                  as Batch
import qualified Flowbox.Batch.Handler.Graph                   as Batch
import qualified Flowbox.Batch.Handler.Library                 as Batch
import qualified Flowbox.Batch.Handler.NodeDefault             as Batch
import qualified Flowbox.Batch.Handler.Project                 as Batch
import qualified Flowbox.Config.Config                         as Config
import           Flowbox.Control.Error
import qualified Flowbox.Data.Graph                            as Graph
import           Flowbox.Data.Version                          ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.UniPath                        as UniPath
import qualified Luna.DEP.AST.Arg                              as Arg
import qualified Luna.DEP.AST.Control.Crumb                    as Crumb
import qualified Luna.DEP.AST.Expr                             as Expr
import qualified Luna.DEP.AST.Lit                              as Lit
import qualified Luna.DEP.AST.Name                             as Name
import qualified Luna.DEP.AST.Type                             as Type
import qualified Luna.DEP.Graph.Flags                          as Flags
import           Luna.DEP.Graph.Node                           (Node)
import qualified Luna.DEP.Graph.Node                           as Node
import qualified Luna.DEP.Graph.Node.Expr                      as NodeExpr
import qualified Luna.DEP.Graph.Node.StringExpr                as StringExpr
import qualified Luna.DEP.Graph.PropertyMap                    as PropertyMap
import qualified Luna.DEP.Pass.Analysis.Alias.Alias            as Alias
import qualified Luna.DEP.Pass.Transform.Graph.Builder.Builder as GraphBuilder



main :: IO ()
main = hspec spec


runBatch :: Batch.Batch a -> IO a
runBatch batch = do
    config <- Config.load
    eitherStringToM' $ Batch.runBatch (Batch.make config) batch


shouldBe' :: (Show a, Eq a, MonadIO m) => a -> a -> m ()
shouldBe' = liftIO .: shouldBe


stringNode :: String -> Node
stringNode str = Node.Expr (NodeExpr.StringExpr $ StringExpr.fromString str) def def


rootLogger :: Logger
rootLogger = getLogger ""


shouldSatisfy' :: (MonadIO m, Show a) => a -> (a -> Bool) -> m ()
shouldSatisfy' = liftIO .: shouldSatisfy


spec :: Spec
spec = do
    describe "node defaults" $
        it "generated nodes are marked generated" $ do
            --rootLogger setIntLevel 5
            runBatch $ do
                (projectID, _) <- Batch.createProject (Just "TestProject") (UniPath.fromUnixString "/tmp/test.project") def
                (libraryID, _) <- Batch.createLibrary "Main" def (UniPath.fromUnixString "/tmp/test.lunalib") projectID
                let mainFun    = Expr.Function 0 [] (Name.single "main" ) [] (Type.Unknown 0) []
                    mainBC     = [Crumb.Module "Main", Crumb.Function (Name.single "main") []]
                _              <- Batch.addFunction mainFun [Crumb.Module "Main"] libraryID projectID
                let testFun    = Expr.Function 0 [] (Name.single "test" ) [] (Type.Unknown 0) []
                    testBC     = [Crumb.Module "Main", Crumb.Function (Name.single "test") []]
                _              <- Batch.addFunction testFun [Crumb.Module "Main"] libraryID projectID

                printID        <- Batch.addNode (stringNode "print") mainBC libraryID projectID
                --putStrLn "= 1 ======================="
                --prettyPrint   =<< Batch.getLibrary libraryID projectID
                --putStrLn "= 2 ======================="
                let printDef   = NodeExpr.ASTExpr $ Expr.App 0 (Expr.Accessor 0 (Expr.mkAccessor "foo") $
                                                                                 Expr.List 0 [ Expr.App 0 (Expr.Con 0 "Foo") [Arg.Unnamed 0 $ Expr.Lit 0 $ Lit.String 0 "fooo"]])
                                                               [ Arg.Unnamed 0 $ Expr.App 0 (Expr.Con 0 "Bar") [Arg.Unnamed 0 $ Expr.Lit 0 $ Lit.String 0 "bar"]
                                                               , Arg.Unnamed 0 $ Expr.Lit 0 $ Lit.String 0 "a"
                                                               , Arg.Unnamed 0 $ Expr.Lit 0 $ Lit.String 0 "b"
                                                               ]
                    anotherDef = NodeExpr.ASTExpr $ Expr.List 0 []
                anotherDefID   <- Batch.setNodeDefault [0] anotherDef printID mainBC libraryID projectID
                printDefID     <- Batch.setNodeDefault [0] printDef printID mainBC libraryID projectID
                testID         <- Batch.addNode (stringNode "test") mainBC libraryID projectID
                ast            <- Batch.getAST libraryID projectID
                propertyMap    <- Batch.getPropertyMap libraryID projectID
                expr           <- Batch.getFunctionFocus mainBC libraryID projectID
                aa             <- EitherT $ Alias.run ast
                (graph, pm)    <- EitherT $ GraphBuilder.run aa propertyMap False expr
                let nodes              = Graph.nodes graph
                    generatedNodes     = nodes \\ [-3,-2, printID, testID]
                    isGenerated nodeID = Flags.isDefaultNodeGenerated $ PropertyMap.getFlags nodeID pm
                mapM_ (`shouldSatisfy'` isGenerated) generatedNodes
