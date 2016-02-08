{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Compilation.Passes.Inference.Literals
    ( assignLiteralTypes
    , Foo
    ) where

import           Prelude.Luna                                    hiding (Num, pre)

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


data Foo = Foo deriving (Show)

pre :: ( ls   ~ NetLayers Foo
       , term ~ Draft Static
       , ne   ~ Link (ls :< term)
       , Castable e ne
       , MonadBuilder n e m
       , NodeInferable m (ls :< term)
       , TermNode Cons m (ls :< term)
       )
    => Ref (Node $ (ls :< term))
    -> m [Ref (Node $ (ls :< term))]
pre ref = do
    node <- read ref
    let inputs = node # Inputs
    mapM (follow source) inputs

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
                   => Ref (Node $ (ls :< term))
                   -> m ()
assignLiteralTypes ref = do
    (consIntRef, consStrRef) <- createLiteralTypes
    assignLiteralTypesWith consIntRef consStrRef ref

assignLiteralTypesWith :: ( ls   ~ NetLayers Foo
                          , term ~ Draft Static
                          , ne   ~ Link (ls :< term)
                          , Castable e ne
                          , MonadIO m
                          , MonadBuilder n e m
                          , NodeInferable m (ls :< term)
                          , TermNode Cons m (ls :< term)
                          , TermNode Lam  m (ls :< term)
                          )
                       => Ref (Node $ (ls :< term))
                       -> Ref (Node $ (ls :< term))
                       -> Ref (Node $ (ls :< term))
                       -> m ()
assignLiteralTypesWith consIntRef consStrRef ref = do
    node <- read ref
    caseTest (uncover node) $ do
        match $ \(Str str) -> void $ reconnect ref (prop Type) consStrRef
        match $ \(Num num) -> void $ reconnect ref (prop Type) consIntRef
        match $ \ANY       -> return ()
    mapM_ (assignLiteralTypesWith consIntRef consStrRef) =<< pre ref

createLiteralTypes :: ( ls   ~ NetLayers Foo
                      , term ~ Draft Static
                      , ne   ~ Link (ls :< term)
                      , NodeInferable m (ls :< term)
                      , TermNode Cons m (ls :< term)
                      )
                   => m (Ref (Node $ (ls :< term)), Ref (Node $ (ls :< term)))
createLiteralTypes = do
    consIntRef <- cons "Int"
    consStrRef <- cons "String"
    return (consIntRef, consStrRef)
