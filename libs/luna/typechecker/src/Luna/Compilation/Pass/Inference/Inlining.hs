{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Inference.Inlining where

import Prelude.Luna

import Prelude.Luna

import Data.Construction
import Data.Container                               hiding (impossible)
import Data.Prop
import Data.Record
import Luna.Evaluation.Runtime                      (Static, Dynamic)
import Luna.Library.Symbol.Class                    (MonadSymbol, lookupSymbol)
import Luna.Syntax.AST.Decl.Function                (Function, FunctionPtr)
import Luna.Syntax.AST.Term                         hiding (source)
import Luna.Syntax.Model.Graph
import Luna.Syntax.Model.Graph.Builder              hiding (run)
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder            (merge)
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Luna.Syntax.Name.Ident.Pool                  (MonadIdentPool, newVarIdent')
import Type.Inference

import qualified Data.Map as Map

import qualified Luna.Library.Symbol.QualPath     as QualPath
import qualified Luna.Syntax.AST.Decl.Function    as Function
import qualified Luna.Syntax.Model.Graph          as Graph
import qualified Luna.Syntax.Model.Graph.Builder  as Graph



#define PassCtx(m,ls,term) ( term ~ Draft Static                     \
                           , ls   ~ NetLayers a                      \
                           , ne   ~ Link (ls :< term)                \
                           , n    ~ (ls :< term)                     \
                           , Prop Type   (ls :< term) ~ Ref ne       \
                           , BiCastable     e ne                     \
                           , BiCastable     n (ls :< term)           \
                           , MonadBuilder n e (m)                    \
                           , HasProp Type       (ls :< term)         \
                           , NodeInferable  (m) (ls :< term)         \
                           , TermNode Var   (m) (ls :< term)         \
                           , TermNode Acc   (m) (ls :< term)         \
                           , TermNode Cons  (m) (ls :< term)         \
                           , MonadSymbol n  (m)                      \
                           )

getTypeName :: PassCtx(m, ls, term) => Ref (Node $ ls :< term) -> m (Maybe String)
getTypeName ref = do
    node  <- read ref
    tpRef <- follow source $ node # Type
    tp    <- read tpRef
    caseTest (uncover tp) $ do
        match $ \(Cons (Str n)) -> return $ Just n
        match $ \ANY -> return Nothing


lookupFunction :: PassCtx(m, ls, term) => Ref (Node $ ls :< term) -> m (Maybe $ Function n)
lookupFunction ref = do
    node <- read ref
    caseTest (uncover node) $ do
        match $ \(Acc (Str n) t) -> do
            tpName <- getTypeName =<< follow source t
            let symbolName = (<> "." <> n) <$> tpName
            case tpName of
                Just tn -> lookupSymbol $ QualPath.mk symbolName
                Nothing -> return Nothing
        match $ \(Var (Str n)) ->
            lookupSymbol $ QualPath.mk n
        match $ \ANY -> return Nothing

inlineFunction :: PassCtx(m, ls, term) => Function n -> m (FunctionPtr n)
inlineFunction fun = do
    translations <- merge $ fun ^. Function.graph
    let unsafeTranslate i = fromJust $ Map.lookup i translations
    return $ fun ^. Function.fptr & over (Function.self . mapped) unsafeTranslate
                                  & over (Function.args . mapped) unsafeTranslate
                                  & over Function.out unsafeTranslate
