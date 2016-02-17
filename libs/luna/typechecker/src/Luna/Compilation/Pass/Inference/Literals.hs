{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Inference.Literals where

import           Prelude.Luna                                    hiding (Num, pre)

import           Data.Construction
import           Data.Prop
import           Data.Record                                     hiding (cons)
import           Type.Inference
import           Data.Graph.Builder
import           Data.Graph.Backend.VectorGraph                  as Graph

import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import           Luna.Syntax.AST.Term                            hiding (source)
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Class    (arg)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Class                 ()
import           Luna.Syntax.Model.Network.Term


#define PassCtx(m, ls, term) ( ls   ~ NetLayers a                          \
                             , term ~ Draft Static                         \
                             , ne   ~ Link (ls :<: term)                    \
                             , BiCastable    e ne                          \
                             , BiCastable    n (ls :<: term)                \
                             , MonadIO m                                   \
                             , MonadBuilder (Hetero (VectorGraph n e c)) m \
                             , NodeInferable m (ls :<: term)                \
                             , TermNode Cons m (ls :<: term)                \
                             , TermNode Lam  m (ls :<: term)                \
                             )

assignLiteralType :: PassCtx(m, ls, term)
                  => Ref Node (ls :<: term)
                  -> Ref Node (ls :<: term)
                  -> Ref Node (ls :<: term)
                  -> m ()
assignLiteralType consIntRef consStrRef ref = do
    node <- read ref
    caseTest (uncover node) $ do
        let process = void ∘ reconnect ref (prop Type)
        match $ \(Str str) -> process consStrRef
        match $ \(Num num) -> process consIntRef
        match $ \ANY       -> return ()

createLiteralTypes :: PassCtx(m, ls, term) => m (Ref Node (ls :<: term), Ref Node (ls :<: term))
createLiteralTypes = do
    consIntRef <- cons "Int"
    consStrRef <- cons "String"
    return (consIntRef, consStrRef)

run :: PassCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m ()
run literals = do
    (consIntRef, consStrRef) <- createLiteralTypes
    mapM_ (assignLiteralType consIntRef consStrRef) literals
