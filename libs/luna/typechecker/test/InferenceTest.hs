{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Prelude.Luna                                    hiding (Num)

import           Luna.Compilation.Pass.Inference.Literals
import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Evaluation.Runtime                    (Dynamic, Static)
import           Luna.Syntax.AST.Term                       hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target)
import qualified Luna.Syntax.AST.Term                       as Term
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Class    (arg)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Term

import           Data.Construction
import           Data.Prop
import           Data.Record                                     hiding (cons)
import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Syntax.AST.Term                            hiding (source)
import           Luna.Syntax.Model.Graph.Builder
import           Luna.Syntax.Model.Network.Class                 ()
import           Type.Inference

import           Luna.Compilation.Pass.Inference.Literals      (assignLiteralTypes)



data Foo = Foo deriving (Show)

renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)

graph1 :: ( ls   ~ NetLayers Foo
          , term ~ Draft Static
          , MonadIO       m
          , NodeInferable m (ls :< term)
          , TermNode Star m (ls :< term)
          , TermNode Var  m (ls :< term)
          , TermNode Num  m (ls :< term)
          , TermNode Str  m (ls :< term)
          , TermNode Acc  m (ls :< term)
          , TermNode App  m (ls :< term)
          )
       => m (Ref (Node $ (ls :< term)))
graph1 = do
    i1 <- int 2
    i2 <- int 3
    i3 <- int 4
    s1 <- str "abc"
    s2 <- str "def"
    s3 <- str "ghi"

    accPlus1a  <- acc "+" i1
    appPlus1a  <- app accPlus1a [arg i2]

    accPlus1b  <- acc "+" i3
    appPlus1b  <- app accPlus1b [arg appPlus1a]

    accConc1a  <- acc "++" s2
    appConc1a  <- app accConc1a [arg s1]

    accConc1b  <- acc "++" appConc1a
    appConc1b  <- app accConc1b [arg s3]

    accLen     <- acc "len" appConc1b
    appLen     <- app accLen []

    accPlus2   <- acc "+" appPlus1b
    appPlus2   <- app accPlus2 [arg appLen]

    -- print appPlus2
    -- return i2
    return appPlus2
    -- return i1

prebuild :: IO (Ref $ Node (NetLayers Foo :< Draft Static), NetGraph Foo)
prebuild = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
         $ runNetworkBuilderT def
         $ star


buildBase :: NetGraph Foo -> IO (Ref (Node $ (NetLayers Foo :< Draft Static)), NetGraph Foo)
buildBase g = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
            $ runNetworkBuilderT g graph1

-- runPass :: NetGraph Foo -> IO (Ref (Node $ (NetLayers Foo :< Draft Static)), NetGraph Foo)
runPass g m = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
            $ runNetworkBuilderT g m

main = do
    (_, g)   <- prebuild
    (f, g')  <- buildBase g
    (_, g'') <- runPass g' (assignLiteralTypes f)
    renderAndOpen [("g", g'')]
    putStrLn "done"
