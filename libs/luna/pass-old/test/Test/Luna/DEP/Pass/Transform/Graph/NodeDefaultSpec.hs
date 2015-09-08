---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.DEP.Pass.Transform.Graph.NodeDefaultSpec where

import Control.Monad (forM_)
import Test.Hspec

import           Flowbox.Prelude
import qualified Luna.DEP.Pass.Transform.GraphView.Defaults       as Defaults
import qualified Test.Luna.DEP.Pass.Transform.Graph.GraphViewSpec as GVTest
import           Test.Luna.DEP.Sample.NodeDefault                 (sampleGraphs)
--import Flowbox.Control.Error
--import qualified Luna.Graph.View.GraphView                    as GraphView
--import qualified Test.Luna.Pass.Transform.Graph.Common        as Common
--import qualified Test.Luna.AST.Common                    as Common
--import qualified Test.Luna.Sample.Code                   as SampleCode



main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "graph node defaults" $ do
        forM_ sampleGraphs $ \(name, graphview, properties) -> do
            it ("adds and removes from graph on demand - " ++ name) $ do
                let grp = (graphview, properties)
                    grpAdded               = uncurry Defaults.addDefaults    grp
                    grpAddedAdded          = uncurry Defaults.addDefaults    grpAdded
                    grpAddedRemoved        = uncurry Defaults.removeDefaults grpAdded
                    grpAddedRemovedRemoved = uncurry Defaults.removeDefaults grpAddedRemoved
                --prettyPrint grpAdded
                grp             `shouldBe` grpAddedRemoved
                grpAdded        `shouldBe` grpAddedAdded
                grpAddedRemoved `shouldBe` grpAddedRemovedRemoved
                uncurry GVTest.backAndForth2 grpAdded
                --(graph, prop) <- eitherStringToM $ uncurry GraphView.toGraph grpAdded
                --let bc = Common.mainBC
                --emptyAst  <- Common.getAST SampleCode.emptyMain
                --(ast, pm) <- Common.getExpr bc graph prop emptyAst
                --prettyPrint (ast, pm)
                --cAdded'    <- Common.getGraph bc pm ast
                --prettyPrint cAdded'
                --let cAdded = uncurry GraphView.fromGraph cAdded'

                --let cAddedRemoved = uncurry Defaults.removeDefaults cAdded
                ----prettyPrint grp
                ----print "=================="
                ----prettyPrint cAdded
                ----print "------------------"
                ----prettyPrint cAddedRemoved
                --grp `shouldBe` cAddedRemoved

