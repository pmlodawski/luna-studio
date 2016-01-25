{-# LANGUAGE GADTs                     #-}

module Luna.Inference.Literals where

import           Control.Monad              (forM_)
import qualified Data.IntSet                as IntSet
import           Data.Layer.Coat            (uncoat, Coat, Uncoated, Coated)
import           Data.Variants              (match, case', ANY(..))
import           Development.Placeholders
import           Prologue                   hiding (pre, succ, cons)
import qualified Data.Text.Lazy             as Text

import           Luna.Interpreter.Label     (Label)
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


succ :: BuilderMonad (Network Label) m => Ref Node -> m [Ref Node]
succ ref = do
    node <- B.readRef ref
    mapM (B.unfollow . Ref . Edge) $ IntSet.toList $ node ^. G.succs


typed a t = StarBuilder.with (const $ Just t) a

getConsInt :: (StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m,
               NodeBuilder.MonadNodeBuilder (Ref Node) m,
               MonadFix m,
               MonadIO m,
               BuilderMonad (Network Label) m) => m (Ref Node)
getConsInt = do
    nameInt <- B._string "Int"
    consInt <- B.cons nameInt
    return consInt

getConsString :: (StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m,
                  NodeBuilder.MonadNodeBuilder (Ref Node) m,
                  MonadFix m,
                  MonadIO m,
                  BuilderMonad (Network Label) m) => m (Ref Node)
getConsString = do
    nameString <- B._string "String"
    consString <- B.cons nameString
    return consString

assignLiteralTypes :: (StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m,
                       NodeBuilder.MonadNodeBuilder (Ref Node) m,
                       MonadFix m,
                       MonadIO m,
                       BuilderMonad (Network Label) m) => Ref Node -> m ()
assignLiteralTypes ref = do
    consInt <- getConsInt
    consString <- getConsString
    assignLiteralTypesWithTypes consInt consString ref

assignLiteralTypesWithTypes :: (StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m,
                       NodeBuilder.MonadNodeBuilder (Ref Node) m,
                       MonadFix m,
                       MonadIO m,
                       BuilderMonad (Network Label) m) => Ref Node -> Ref Node -> Ref Node -> m ()
assignLiteralTypesWithTypes consInt consString ref = do
    node <- B.readRef ref
    case' (uncoat node) $ do
        match $ \(Val val) -> do
            case' val $ match $ \lit -> case lit of
                Lit.Int    i -> do
                    putStrLn $ "Lit.Int " <> show i <> ": " <> show node
                    tnode <- B.follow $ node ^. Typed.tp
                    B.reconnect ref Typed.tp consInt
                    return ()
                Lit.String s -> do
                    putStrLn $ "Lit.String " <> show s <> ": " <> show node
                    tnode <- B.follow $ node ^. Typed.tp
                    B.reconnect ref Typed.tp consString
                    return ()
                _            -> do
                    putStrLn $ "ANY?: " <> show node
        match $ \ANY -> do
            putStrLn $ "ANY: " <> show node
            return ()
    mapM_ (assignLiteralTypesWithTypes consInt consString) =<< succ ref
