{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Prelude.Luna                                    hiding (Num)

import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Term

import           Data.Construction
import           Data.Prop
import           Data.Record
import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Syntax.AST.Term                            hiding (source)
import           Luna.Syntax.Model.Graph.Builder
import           Luna.Syntax.Model.Network.Class                 ()
import           Type.Inference




renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)


data Foo = Foo deriving (Show)

--var :: NodeBuilder Var m (ls :< term) => NameInput (Ref (Node $ ls :< term)) -> m (Ref (Node $ ls :< term))


myg :: ( ls   ~ NetLayers Foo
       , term ~ Draft Static
       , MonadIO       m
       , NodeInferable m (ls :< term)
       , TermNode Star m (ls :< term)
       , TermNode Var  m (ls :< term)
       , TermNode Num  m (ls :< term)
       , TermNode Str  m (ls :< term)
       , TermNode App  m (ls :< term)
       )
    => m (Ref (Node $ (ls :< term)))
myg = do
    i1 <- int 2
    i2 <- int 3
    i3 <- int 4
    s1 <- str "abc"
    s2 <- str "def"
    s3 <- str "ghi"
    return i1

    -- a  <- var' "a"
    -- b  <- var' "b"
    -- r  <- app' f [arg a, arg b]


universe = Ref $ Ptr 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?

assignLiteralTypes :: ( ls   ~ NetLayers Foo
                , term ~ Draft Static
                , ne   ~ Link (ls :< term)
                , Castable e ne
                , MonadIO m
                , MonadBuilder n e m
                , NodeInferable m (ls :< term)
                , TermNode Var  m (ls :< term)
                , TermNode Lam  m (ls :< term)
                ) => Ref (Node $ (ls :< term)) -> m ()
assignLiteralTypes appRef = do
    appNode <- read appRef
    -- caseTest (uncover appNode) $ do
    --     match $ \(App srcConn argConns) -> do
    --         args     <- (mapM . mapM) (follow source) argConns
    --         specArgs <- (mapM . mapM) getTypeSpec args
    --         out      <- var' "out"
    --         l        <- lam' specArgs out

    --         reconnect appRef (prop Type) out

    --         src     <- follow source srcConn
    --         reconnect src (prop Type) l
    --         return ()

    --     match $ \ANY -> impossible

    return ()


prebuild :: IO (Ref $ Node (NetLayers Foo :< Draft Static), NetGraph Foo)
prebuild = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
         $ runNetworkBuilderT def
         $ do
    star


buildBase :: NetGraph Foo -> IO (Ref (Node $ (NetLayers Foo :< Draft Static)), NetGraph Foo)
buildBase g = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
       $ runNetworkBuilderT g myg

-- runPass :: NetGraph Foo -> IO (Ref (Node $ (NetLayers Foo :< Draft Static)), NetGraph Foo)
runPass g m = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
        $ runNetworkBuilderT g m

main = do
    (_, g)   <- prebuild
    (f, g')  <- buildBase g
    (_, g'') <- runPass g' (assignLiteralTypes f)
    renderAndOpen [("g", g')]
    print "done"
