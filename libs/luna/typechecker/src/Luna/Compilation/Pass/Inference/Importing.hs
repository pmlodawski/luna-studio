{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Compilation.Pass.Inference.Importing where

import Prelude.Luna

import Control.Monad.Error                          (throwError, ErrorT, runErrorT, Error)
import Data.Construction
import Data.Prop
import Data.Record
import Data.Maybe                                   (fromMaybe)
import Luna.Evaluation.Runtime                      (Static, Dynamic)
import Luna.Library.Symbol.Class                    (MonadSymbol, lookupFunction, lookupLambda, loadLambda)
import Luna.Syntax.AST.Decl.Function                (Function, FunctionPtr, Lambda (..))
import Luna.Syntax.AST.Term                         hiding (source)
import Data.Graph.Builder                           as Graph hiding (run)
import Data.Graph.Backend.VectorGraph               as Graph
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder            (merge, dupCluster)
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers, NetCluster)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Type.Inference

import qualified Data.Map as Map

import qualified Luna.Library.Symbol.QualPath     as QualPath
import qualified Luna.Syntax.AST.Decl.Function    as Function



#define PassCtx forall node edge ls term graph e n m a c. \
                ( term  ~ Draft Static                    \
                , ls    ~ NetLayers a                     \
                , edge  ~ Link (ls :<: term)              \
                , node  ~ (ls :<: term)                   \
                , c     ~ NetCluster                      \
                , graph ~ Hetero (VectorGraph n e c)      \
                , BiCastable     e edge                   \
                , BiCastable     n node                   \
                , MonadBuilder graph (m)                  \
                , NodeInferable  (m) (ls :<: term)        \
                , TermNode Var   (m) (ls :<: term)        \
                , TermNode Acc   (m) (ls :<: term)        \
                , TermNode Cons  (m) (ls :<: term)        \
                , TermNode Lam   (m) (ls :<: term)        \
                , TermNode Unify (m) (ls :<: term)        \
                , MonadSymbol node graph (m)              \
                , Referred Node n graph                   \
                )

data ImportError = NotABindingNode | AmbiguousNodeType | SymbolNotFound deriving (Show)
instance Error ImportError

type ImportErrorT = ErrorT ImportError

getTypeName :: PassCtx => Ref Node node -> ImportErrorT m String
getTypeName ref = do
    node  <- read ref
    tpRef <- follow source $ node # Type
    tp    <- read tpRef
    caseTest (uncover tp) $ do
        match $ \(Cons (Str n)) -> return n
        match $ \ANY -> throwError AmbiguousNodeType

getFunctionName :: PassCtx => Ref Node node -> ImportErrorT m String
getFunctionName ref = do
    node <- read ref
    caseTest (uncover node) $ do
        match $ \(Acc (Str n) t) -> do
            tpName <- getTypeName =<< follow source t
            return $ tpName <> "." <> n
        match $ \(Var (Str n)) -> return n
        match $ \ANY -> throwError NotABindingNode

funLookup :: PassCtx => String -> ImportErrorT m (Function node graph)
funLookup name = do
    f <- lookupFunction $ QualPath.mk name
    fromMaybe (throwError SymbolNotFound) (return <$> f)

importFunction :: PassCtx => String -> Function node graph -> ImportErrorT m (Lambda node)
importFunction name fun = do
    translations <- merge $ fun ^. Function.graph
    cls :: Ref Cluster c <- subgraph
    withRef cls $ prop Name .~ name
    mapM (flip include cls) $ Map.elems $ translations
    let unsafeTranslate i = fromJust $ Map.lookup i translations
        fptr = fun ^. Function.fptr & over (Function.self . mapped) unsafeTranslate
                                    & over (Function.args . mapped) unsafeTranslate
                                    & over Function.out unsafeTranslate
    loadLambda (QualPath.mk name) $ Lambda fptr ()
    return $ Lambda fptr ()

buildTypeRep :: PassCtx => FunctionPtr node -> m (Ref Node node)
buildTypeRep fptr = do
    argTypes <- mapM (follow (prop Type) >=> follow source) $ fptr ^. Function.args
    outType  <- follow (prop Type) (fptr ^. Function.out) >>= follow source
    case argTypes of
        [] -> return outType
        as -> lam (arg <$> as) outType

processNode :: PassCtx => Ref Node node -> m (Either ImportError ())
processNode ref = runErrorT $ do
    name   <- getFunctionName ref
    lambda <- lookupLambda $ QualPath.mk name
    lamb <- case lambda of
        Just l  -> return l
        Nothing -> do
            fun <- funLookup name
            importFunction name fun
    return ()
    {-fun       <- MaybeT $ lookupFunction ref-}
    {-Lambda fptr _ <- lift $ importFunction fun-}
    {-tpRep <- lift $ buildTypeRep fptr-}
    {-refTp <- follow (prop Type) ref >>= follow source-}
    {-uni   <- lift $ unify refTp tpRep-}
    {-return fptr-}
