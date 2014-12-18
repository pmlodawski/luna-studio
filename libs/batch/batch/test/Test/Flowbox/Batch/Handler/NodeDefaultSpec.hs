---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}

module Test.Flowbox.Batch.Handler.NodeDefaultSpec where

import Test.Hspec

import qualified Flowbox.Batch.Batch               as Batch
import qualified Flowbox.Batch.Handler.AST         as AST
import qualified Flowbox.Batch.Handler.Common      as Batch
import qualified Flowbox.Batch.Handler.Graph       as Graph
import qualified Flowbox.Batch.Handler.Library     as Library
import qualified Flowbox.Batch.Handler.NodeDefault as NodeDefault
import qualified Flowbox.Batch.Handler.Project     as Project
import qualified Flowbox.Config.Config             as Config
import           Flowbox.Control.Error
import           Flowbox.Data.Version              ()
import           Flowbox.Prelude
import qualified Flowbox.System.UniPath            as UniPath
import qualified Luna.AST.Arg                      as Arg
import qualified Luna.AST.Control.Crumb            as Crumb
import qualified Luna.AST.Expr                     as Expr
import qualified Luna.AST.Lit                      as Lit
import qualified Luna.AST.Name                     as Name
import qualified Luna.AST.Type                     as Type
import           Luna.Graph.Node                   (Node)
import qualified Luna.Graph.Node                   as Node
import qualified Luna.Graph.Node.Expr              as NodeExpr
import qualified Luna.Graph.Node.StringExpr        as StringExpr
import           Flowbox.System.Log.Logger



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



spec :: Spec
spec = do
    describe "node defaults" $
        it "sample use case" $ do
            --rootLogger setIntLevel 5
            runBatch $ do
                (projectID, _) <- Project.createProject (Just "TestProject") (UniPath.fromUnixString "/tmp/test.project") def
                (libraryID, _) <- Library.createLibrary "Main" def (UniPath.fromUnixString "/tmp/test.lunalib") projectID
                let mainFun    = Expr.Function 0 [] (Name.single "main" ) [] (Type.Unknown 0) []
                    mainBC     = [Crumb.Module "Main", Crumb.Function (Name.single "main") []]
                _              <- AST.addFunction mainFun [Crumb.Module "Main"] libraryID projectID
                let testFun    = Expr.Function 0 [] (Name.single "test" ) [] (Type.Unknown 0) []
                    testBC     = [Crumb.Module "Main", Crumb.Function (Name.single "test") []]
                _              <- AST.addFunction testFun [Crumb.Module "Main"] libraryID projectID

                printID        <- Graph.addNode (stringNode "print") mainBC libraryID projectID
                putStrLn "= 1 ======================="
                prettyPrint   =<< Batch.getLibrary libraryID projectID
                putStrLn "= 2 ======================="
                let printDef   = NodeExpr.ASTExpr $ Expr.App 0 (Expr.Accessor 0 (Expr.mkAccessor "foo") $
                                                                                 Expr.List 0 [ Expr.App 0 (Expr.Con 0 "Foo") [Arg.Unnamed 0 $ Expr.Lit 0 $ Lit.String 0 "fooo"]])
                                                               [ Arg.Unnamed 0 $ Expr.App 0 (Expr.Con 0 "Bar") [Arg.Unnamed 0 $ Expr.Lit 0 $ Lit.String 0 "bar"]
                                                               , Arg.Unnamed 0 $ Expr.Lit 0 $ Lit.String 0 "a"
                                                               , Arg.Unnamed 0 $ Expr.Lit 0 $ Lit.String 0 "b"
                                                               ]
                printDefID     <- NodeDefault.setNodeDefault [0] printDef printID mainBC libraryID projectID
                prettyPrint   =<< Batch.getLibrary libraryID projectID
                testID         <- Graph.addNode (stringNode "test") mainBC libraryID projectID
                putStrLn "= 3 ======================="
                prettyPrint   =<< Batch.getLibrary libraryID projectID
                --fun1           <- Batch.getFunctionFocus mainBC libraryID projectID
                --prettyPrint   =<< Batch.getLibrary libraryID projectID
                --library1       <- Batch.getLibrary libraryID projectID
                --testID         <- Graph.addNode (stringNode "test") testBC libraryID projectID
                --fun2           <- Batch.getFunctionFocus mainBC libraryID projectID
                --fun1 `shouldBe'` fun2
                --prettyPrint   =<< Batch.getLibrary libraryID projectID
                --library2       <- Batch.getLibrary libraryID projectID
                --graph          <- Graph.nodesGraph mainBC libraryID projectID
                --library1 `shouldBe'` library2
                return ()
