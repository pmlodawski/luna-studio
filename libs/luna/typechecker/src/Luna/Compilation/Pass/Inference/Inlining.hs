{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Inference.Inlining where

import Prelude.Luna

import Control.Monad.Trans.Maybe                    (runMaybeT, MaybeT (..))
import Data.Construction
import Data.Prop
import Data.Record
import Luna.Evaluation.Runtime                      (Static, Dynamic)
import Luna.Library.Symbol.Class                    (MonadSymbol, lookupSymbol)
import Luna.Syntax.AST.Decl.Function                (Function, FunctionPtr, Lambda (..))
import Luna.Syntax.AST.Term                         hiding (source)
import Data.Graph.Builder                           as Graph hiding (run)
import Data.Graph.Backend.VectorGraph               as Graph
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder            (merge)
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Type.Inference

import qualified Data.Map as Map

import qualified Luna.Library.Symbol.QualPath     as QualPath
import qualified Luna.Syntax.AST.Decl.Function    as Function



#define PassCtx ( term  ~ Draft Static             \
                , ls    ~ NetLayers a              \
                , edge  ~ Link (ls :< term)        \
                , node  ~ (ls :< term)             \
                , graph ~ Hetero (VectorGraph n e) \
                , BiCastable     e edge            \
                , BiCastable     n node            \
                , MonadBuilder graph (m)           \
                , NodeInferable  (m) (ls :< term)  \
                , TermNode Var   (m) (ls :< term)  \
                , TermNode Acc   (m) (ls :< term)  \
                , TermNode Cons  (m) (ls :< term)  \
                , TermNode Lam   (m) (ls :< term)  \
                , TermNode Unify (m) (ls :< term)  \
                , MonadSymbol node graph (m)       \
                , Referred Node n graph            \
                )

getTypeName :: PassCtx => Ref Node node -> m (Maybe String)
getTypeName ref = do
    node  <- read ref
    tpRef <- follow source $ node # Type
    tp    <- read tpRef
    caseTest (uncover tp) $ do
        match $ \(Cons (Str n)) -> return $ Just n
        match $ \ANY -> return Nothing


lookupFunction :: PassCtx => Ref Node node -> m (Maybe $ Function node graph)
lookupFunction ref = do
    node <- read ref
    caseTest (uncover node) $ do
        match $ \(Acc (Str n) t) -> do
            tpName <- getTypeName =<< follow source t
            let symbolName = (<> "." <> n) <$> tpName
            case symbolName of
                Just tn -> lookupSymbol $ QualPath.mk tn
                Nothing -> return Nothing
        match $ \(Var (Str n)) ->
            lookupSymbol $ QualPath.mk n
        match $ \ANY -> return Nothing

importFunction :: PassCtx => Function node graph -> m (Lambda node)
importFunction fun = do
    translations <- merge $ fun ^. Function.graph
    cls <- subgraph "myfun"
    mapM (flip include cls) $ Map.elems $ translations
    let unsafeTranslate i = fromJust $ Map.lookup i translations
        fptr = fun ^. Function.fptr & over (Function.self . mapped) unsafeTranslate
                                    & over (Function.args . mapped) unsafeTranslate
                                    & over Function.out unsafeTranslate
    return $ Lambda fptr cls

buildTypeRep :: PassCtx => FunctionPtr node -> m (Ref Node node)
buildTypeRep fptr = do
    argTypes <- mapM (follow (prop Type) >=> follow source) $ fptr ^. Function.args
    outType  <- follow (prop Type) (fptr ^. Function.out) >>= follow source
    case argTypes of
        [] -> return outType
        as -> lam (arg <$> as) outType

processNode :: PassCtx => Ref Node node -> m (Maybe $ FunctionPtr node)
processNode ref = runMaybeT $ do
    fun       <- MaybeT $ lookupFunction ref
    Lambda fptr _ <- lift $ importFunction fun
    {-tpRep <- lift $ buildTypeRep fptr-}
    {-refTp <- follow (prop Type) ref >>= follow source-}
    {-uni   <- lift $ unify refTp tpRep-}
    return fptr
