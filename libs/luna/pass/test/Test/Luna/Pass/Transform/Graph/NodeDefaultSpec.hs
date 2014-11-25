---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Pass.Transform.Graph.NodeDefaultSpec where

import Control.Monad         (forM_)
import Flowbox.Control.Error
import Test.Hspec

import           Flowbox.Prelude
import qualified Luna.Pass.Transform.GraphView.Defaults       as Defaults
import qualified Test.Luna.Pass.Transform.Graph.GraphViewSpec as GVTest
import           Test.Luna.Sample.NodeDefault                 (sampleGraphs)
--import qualified Luna.Graph.View.GraphView             as GraphView
--import qualified Test.Luna.Pass.Transform.Graph.GraphSpec     as GTest
--import qualified Test.Luna.Pass.Transform.Graph.Common as Common



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
                --GTest.backAndForth2 Common.mainBC graph prop

