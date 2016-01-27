{-# LANGUAGE GADTs                     #-}

module Luna.Inference.Literals
    ( assignLiteralTypes
    ) where

import           Control.Monad              (forM_)
import qualified Data.IntSet                as IntSet
import           Data.Layer.Coat            (uncoat, Coat, Uncoated, Coated)
import           Data.Variants              (match, case', ANY(..))
import           Development.Placeholders
import           Prologue                   hiding (pre, succ, cons)
import qualified Data.Text.Lazy             as Text
import           Data.Construction

import           Luna.Interpreter.Label     (Label)
import qualified Luna.Interpreter.Label     as Label
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Lit        as Lit
import qualified Luna.Syntax.Builder        as B
import           Luna.Syntax.Repr.Graph     (Graph)
import           Luna.Syntax.Repr.Graph     (Ref(..), Node(..), Edge, DoubleArc)

import           Luna.Syntax.Builder.Class  (BuilderMonad)
import qualified Luna.Syntax.Builder.Node   as NodeBuilder
import qualified Luna.Syntax.Builder.Star   as StarBuilder
import qualified Luna.Syntax.Builder.Symbol as SymbolBuilder
import           Luna.Syntax.Layer.Labeled  (label)
import           Luna.Syntax.Repr.Graph     (Edge (Edge), Node, Ref (Ref))
import qualified Luna.Syntax.Repr.Graph     as G
import           Luna.Syntax.Symbol.Network (Network)
import qualified Luna.Syntax.Layer.Labeled  as Labeled
import qualified Luna.Syntax.AST.Typed      as Typed


pre :: BuilderMonad (Network Label) m => Ref Node -> m [Ref Node]
pre ref = do
    node <- B.readRef ref
    mapM (B.follow) $ inputs $ uncoat node

createConsInt :: ( StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m
                 , NodeBuilder.MonadNodeBuilder (Ref Node) m
                 , MonadFix m
                 , MonadIO m
                 , BuilderMonad (Network Label) m
                 ) => m (Ref Node)
createConsInt = do
    nameInt <- B._string "Int"
    B.cons nameInt

createConsString :: ( StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m
                    , NodeBuilder.MonadNodeBuilder (Ref Node) m
                    , MonadFix m
                    , MonadIO m
                    , BuilderMonad (Network Label) m
                    ) => m (Ref Node)
createConsString = do
    nameString <- B._string "String"
    B.cons nameString

assignLiteralTypes :: ( StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m
                      , NodeBuilder.MonadNodeBuilder (Ref Node) m
                      , MonadFix m
                      , MonadIO m
                      , BuilderMonad (Network Label) m
                      ) => Ref Node -> m ()
assignLiteralTypes ref = do
    consIntTpe    <- createConsInt
    consStringTpe <- createConsString
    assignLiteralTypesWithTypes consIntTpe consStringTpe ref

assignLiteralType :: ( StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m
                     , NodeBuilder.MonadNodeBuilder (Ref Node) m
                     , MonadIO m
                     , BuilderMonad (Network Label) m
                     ) => Ref Node -> Ref Node -> m ()
assignLiteralType ref tpe = do
    node <- B.readRef ref
    tnodeRef <- B.follow $ node ^. Typed.tp
    tnode <- B.readRef tnodeRef
    case' (uncoat tnode) $ do
        match $ \Star -> do
            destruct tnodeRef
            void $ B.reconnect ref Typed.tp tpe
        match $ \ANY -> return ()

assignLiteralTypesWithTypes :: ( StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m
                               , NodeBuilder.MonadNodeBuilder (Ref Node) m
                               , MonadIO m
                               , BuilderMonad (Network Label) m
                               ) => Ref Node -> Ref Node -> Ref Node -> m ()
assignLiteralTypesWithTypes consIntTpe consStringTpe ref = do
    a <- get
    putStrln
    node <- B.readRef ref
    -- B.writeRef ref (node & label . Label.checked .~ True)
    case' (uncoat node) $ do
        match $ \(Val val) -> do
            case' val $ match $ \lit -> assignLiteralType ref $ case lit of
                Lit.Int    _ -> consIntTpe
                Lit.String _ -> consStringTpe
        match $ \ANY -> return ()
    mapM_ (assignLiteralTypesWithTypes consIntTpe consStringTpe) =<< pre ref

