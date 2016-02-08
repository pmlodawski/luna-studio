{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Prelude.Luna                                    hiding (Num)

import           Luna.Evaluation.Runtime                         (Dynamic, Static)
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




renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)

data Foo = Foo deriving (Show)

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


universe = Ref $ Ptr 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?

assignLiteralTypes :: ( ls   ~ NetLayers Foo
                      , term ~ Draft Static
                      , ne   ~ Link (ls :< term)
                      , Castable e ne
                      , MonadIO m
                      , MonadBuilder n e m
                      , NodeInferable m (ls :< term)
                      , TermNode Cons m (ls :< term)
                      , TermNode Lam  m (ls :< term)
                      )
                   => Ref (Node $ (ls :< term)) -> m ()
assignLiteralTypes ref = do
    consIntRef <- cons "Int"
    consStrRef <- cons "String"

    node <- read ref
    caseTest (uncover node) $ do
        match $ \(Str str) -> void $ reconnect ref (prop Type) consStrRef
        match $ \(Num num) -> void $ reconnect ref (prop Type) consIntRef
        match $ \ANY       -> return ()
    return ()


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
    renderAndOpen [("g", g')]
    print "done"
