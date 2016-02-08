{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Inference.Struct where

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

import qualified Luna.Compilation.Stage.TypeCheck as TypeCheck
import           Luna.Syntax.Name.Ident.Pool      (MonadIdentPool, newVarIdent')
import qualified Luna.Syntax.Name as Name


#define PassCtx(m,ls,term) ( term ~ Draft Static               \
                           , ne   ~ Link (ls :< term)          \
                           , Prop Type   (ls :< term) ~ Ref ne \
                           , Castable       e ne               \
                           , MonadBuilder n e m                \
                           , HasProp Type     (ls :< term)     \
                           , NodeInferable  m (ls :< term)     \
                           , TermNode Var   m (ls :< term)     \
                           , TermNode Lam   m (ls :< term)     \
                           , MonadIdentPool m                  \
                           )


-- FIXME[WD]: Narrow the Ref type to support only App terms
buildAppType :: PassCtx(m,ls,term) => Ref (Node $ (ls :< term)) -> m ()
buildAppType appRef = do
    appNode <- read appRef
    caseTest (uncover appNode) $ do
        match $ \(App srcConn argConns) -> do
            src      <- follow source srcConn
            args     <- (mapM ∘ mapM) (follow source) argConns
            specArgs <- (mapM ∘ mapM) getTypeSpec args
            out      <- var' =<< newVarIdent'
            l        <- lam' specArgs out
            reconnect appRef (prop Type) out
            reconnect src    (prop Type) l
        match $ \ANY -> impossible
    return ()

-- | Returns a concrete type of a node
--   If the type is just universe, create a new type variable
getTypeSpec :: PassCtx(m,ls,term) => Ref (Node $ (ls :< term)) -> m (Ref (Node $ (ls :< term)))
getTypeSpec ref = do
    val <- read ref
    tp  <- follow source $ val # Type
    if tp /= universe then return tp else do
        ntp <- var' =<< newVarIdent'
        reconnect ref (prop Type) ntp
        return ntp






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




myg :: ( -- ls   ~ NetLayers Foo
        term ~ Draft Static

       , MonadIO       m
       , NodeInferable m (ls :< term)
       , TermNode Star m (ls :< term)
       , TermNode Var  m (ls :< term)
       , TermNode App  m (ls :< term)
       ) => m [Ref (Node $ (ls :< term))]
myg = do
    f  <- var' "f"
    a  <- var' "a"
    b  <- var' "b"
    r  <- app' f [arg a, arg b]

    g  <- var' "g"
    r2 <- app' g [arg r]
    --r_v <- read r
    --print $ caseTest (uncover r_v) $ do
    --    match $ \(App a b) -> "APP!"
    --    match $ \ANY -> "something else :("
    return [r,r2]


universe = Ref $ Ptr 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?


prebuild :: IO (Ref $ Node (NetLayers Foo :< Draft Static), NetGraph Foo)
prebuild = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
         $ runNetworkBuilderT def
         $ do
    star


buildBase :: NetGraph Foo -> IO ([Ref (Node $ (NetLayers Foo :< Draft Static))], NetGraph Foo)
buildBase g = runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
       $ runNetworkBuilderT g myg

--runPass :: NetGraph Foo -> IO (Ref (Node $ (NetLayers Foo :< Draft Static)), NetGraph Foo)
runPass g m = fmap snd
            $ runInferenceT ELEMENT (Proxy :: Proxy (Ref $ Node (NetLayers Foo :< Draft Static)))
            $ runNetworkBuilderT g m

main = do

    (_, g)   <- prebuild
    (fs, g') <- buildBase g
    g'' <- TypeCheck.runT $ runPass g' (mapM buildAppType fs)
    renderAndOpen [ ("g1", g')
                  , ("g2", g'')
                  ]
    print "hej ho!"










--reconnect :: (Reader m src, Writer m src, Connectible (Ref src) (Ref tgt) m, e ~ Connection (Ref src) (Ref tgt), Unregister m e)
--          => Ref src -> Lens' src e -> Ref tgt -> m e
--reconnect srcRef l tgtRef = do
--    src  <- read srcRef
--    unregister $ src ^. l
--    conn <- connection srcRef tgtRef
--    write srcRef $ src & l .~ conn
--    return conn
