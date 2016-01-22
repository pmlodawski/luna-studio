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


succ :: BuilderMonad (Network Label) m => Ref Node -> m [Ref Node]
succ ref = do
    node <- B.readRef ref
    mapM (B.unfollow . Ref . Edge) $ IntSet.toList $ node ^. G.succs


typed a t = StarBuilder.with (const $ Just t) a

type GraphBuilder m a = (Coated a, Uncoated a ~ (Draft (Ref Edge)), BuilderMonad (Graph a DoubleArc) m)

assignLiteralTypes :: (StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m,
                       NodeBuilder.MonadNodeBuilder (Ref Node) m,
                       MonadFix m,
                       BuilderMonad (Network Label) m) => Ref Node -> m ()
assignLiteralTypes ref = do
    nameInt <- B._string "Int"
    consInt <- B.cons nameInt
    nameString <- B._string "String"
    consString <- B.cons nameString
    node <- B.readRef ref
    case' (uncoat node) $ do
        match $ \(Val val) -> do
            case' val $ do
                match $ \(Lit.Int i) -> do
                    typedRef <- B._int i `typed` consInt
                    -- typedNode <- B.readRef typedRef
                    -- B.writeRef ref typedNode
                    -- TODO: do something...
                    return ()
                match $ \(Lit.String s) -> do
                    typedRef <- B._string (Text.unpack . toText $ s) `typed` consString
                    return ()
        match $ \ANY -> return ()
    mapM_ assignLiteralTypes =<< succ ref
