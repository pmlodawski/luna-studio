{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Compilation.Passes.Inference.Struct where

import Prelude.Luna

import Luna.Syntax.Model.Network.Builder.Node.Class
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers)
import Luna.Syntax.Model.Graph
import Luna.Evaluation.Runtime (Static, Dynamic)
import Luna.Syntax.Model.Network.Term
import Luna.Syntax.Model.Layer

import Luna.Syntax.Model.Network.Class ()
import Luna.Diagnostic.Vis.GraphViz


--foo :: forall a. Show a => NetGraph a -> IO (Ref $ Node (NetLayers a :< Draft Static), NetGraph a)
----foo :: NetGraph -> IO ((), NetGraph)
--foo g = runNetworkBuilderT g
--    $ do
--    title "basic element building"
--    s1 <- star
--    print s1

--    return s1


renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)


data Foo = Foo deriving (Show)

myg = do
    s1 <- star
    return s1

prebuild :: IO (Ref $ Node (NetLayers Foo :< Draft Static), NetGraph Foo)
prebuild = runNetworkBuilderT def $ star

pass :: NetGraph Foo -> IO (Ref $ Node (NetLayers Foo :< Draft Static), NetGraph Foo)
pass g = runNetworkBuilderT g myg

main = do

    (_, g)  <- prebuild
    (_, g') <- pass g
    renderAndOpen [("g", g')]
    print "hej ho!"
