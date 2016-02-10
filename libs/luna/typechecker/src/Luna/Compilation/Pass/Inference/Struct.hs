{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Inference.Struct where

import Prelude.Luna

import Data.Construction
import Data.Prop
import Data.Record
import Luna.Evaluation.Runtime                      (Static, Dynamic)
import Luna.Syntax.AST.Term                         hiding (source)
import Luna.Syntax.Model.Graph
import Luna.Syntax.Model.Graph.Builder
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Luna.Syntax.Name.Ident.Pool                  (MonadIdentPool, newVarIdent')
import Type.Inference

import qualified Luna.Compilation.Stage.TypeCheck as TypeCheck
import qualified Luna.Syntax.Name                 as Name


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
            args     <- mapM2 (follow source) argConns
            specArgs <- mapM2 getTypeSpec args
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

run :: PassCtx(m,ls,term) => [Ref (Node $ (ls :< term))] -> m ()
run = void ∘ mapM buildAppType

universe = Ref $ Ptr 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?
