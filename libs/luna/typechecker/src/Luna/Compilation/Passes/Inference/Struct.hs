{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Compilation.Passes.Inference.Struct where

import Prelude.Luna

import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers)
import Luna.Syntax.Model.Graph
import Luna.Evaluation.Runtime (Static, Dynamic)
import Luna.Syntax.Model.Network.Term
import Luna.Syntax.Model.Layer

import Luna.Syntax.Model.Network.Class ()
import Luna.Diagnostic.Vis.GraphViz
import Type.Inference
import Luna.Syntax.Model.Graph.Builder
import Data.Prop
import Data.Construction
import Luna.Syntax.AST.Term hiding (source)
import Data.Record

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

--var :: NodeBuilder Var m (ls :< term) => NameInput (Ref (Node $ ls :< term)) -> m (Ref (Node $ ls :< term))


myg :: ( ls   ~ NetLayers Foo
       , term ~ Draft Static

       , MonadIO       m
       , NodeInferable m (ls :< term)
       , TermNode Star m (ls :< term)
       , TermNode Var  m (ls :< term)
       , TermNode App  m (ls :< term)
       ) => m (Ref (Node $ (ls :< term)))
myg = do
    f  <- var' "f"
    a  <- var' "a"
    b  <- var' "b"
    r  <- app' f [arg a, arg b]

    --r_v <- read r
    --print $ caseTest (uncover r_v) $ do
    --    match $ \(App a b) -> "APP!"
    --    match $ \ANY -> "something else :("
    return r


universe = Ref $ Ptr 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?

-- FIXME[WD]: Narrow the Ref type to support only App terms
buildAppType :: ( ls   ~ NetLayers Foo
                , term ~ Draft Static
                , ne   ~ Link (ls :< term)

                , Castable e ne

                , MonadIO m
                , MonadBuilder n e m
                , NodeInferable m (ls :< term)
                , TermNode Var  m (ls :< term)
                , TermNode Lam  m (ls :< term)
                ) => Ref (Node $ (ls :< term)) -> m ()
buildAppType appRef = do
    appNode <- read appRef
    caseTest (uncover appNode) $ do
        match $ \(App srcConn argConns) -> do
            args     <- (mapM . mapM) (follow source) argConns
            specArgs <- (mapM . mapM) getTypeSpec args
            out      <- var' "out"
            l        <- lam' specArgs out

            reconnect appRef (prop Type) out

            src     <- follow source srcConn
            reconnect src (prop Type) l
            return ()

        match $ \ANY -> impossible

    return ()

getTypeSpec :: ( ls   ~ NetLayers Foo
               , term ~ Draft Static
               , ne   ~ Link (ls :< term)
               , Castable       e ne
               , MonadBuilder n e m
               , NodeInferable m (ls :< term)
               , TermNode Var  m (ls :< term)
               , MonadIO m
               ) => Ref (Node $ (ls :< term)) -> m (Ref (Node $ (ls :< term)))
getTypeSpec ref = do
    val <- read ref
    tp  <- follow source $ val # Type
    if tp /= universe then return tp else do
        ntp <- var' "x"
        reconnect ref (prop Type) ntp
        return ntp


prebuild :: IO (Ref $ Node (NetLayers Foo :< Draft Static), NetGraph Foo)
prebuild = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
         $ runNetworkBuilderT def
         $ do
    star


buildBase :: NetGraph Foo -> IO (Ref (Node $ (NetLayers Foo :< Draft Static)), NetGraph Foo)
buildBase g = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
       $ runNetworkBuilderT g myg

--runPass :: NetGraph Foo -> IO (Ref (Node $ (NetLayers Foo :< Draft Static)), NetGraph Foo)
runPass g m = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
            $ runNetworkBuilderT g m

main = do

    (_, g)   <- prebuild
    (f, g')  <- buildBase g
    (_, g'') <- runPass g' (buildAppType f)
    renderAndOpen [("g", g'')]
    print "hej ho!"




--reconnect :: (Reader m src, Writer m src, Connectible (Ref src) (Ref tgt) m, e ~ Connection (Ref src) (Ref tgt), Unregister m e)
--          => Ref src -> Lens' src e -> Ref tgt -> m e
--reconnect srcRef l tgtRef = do
--    src  <- read srcRef
--    unregister $ src ^. l
--    conn <- connection srcRef tgtRef
--    write srcRef $ src & l .~ conn
--    return conn
