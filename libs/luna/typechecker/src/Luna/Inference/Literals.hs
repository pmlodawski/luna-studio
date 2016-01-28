{-# LANGUAGE GADTs                     #-}

module Luna.Inference.Literals
    ( assignLiteralTypes
    ) where

import           Control.Monad              (forM_)
import qualified Data.IntSet                as IntSet
import           Data.Layer.Coat            (uncoat, Coat, Uncoated, Coated, CoatConstructor)
import           Data.Variants              (match, case', ANY(..))
import           Development.Placeholders
import           Prologue                   hiding (pre, succ, cons)
import qualified Data.Text.Lazy             as Text
import           Data.Construction

import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Lit        as Lit
import qualified Luna.Syntax.Builder        as B
import           Luna.Syntax.Repr.Graph     (Graph)
import           Luna.Syntax.Repr.Graph     (Ref(..), Node(..), Edge, DoubleArc)

import           Luna.Syntax.Builder.Class  (BuilderMonad)
import qualified Luna.Syntax.Builder.Node   as NodeBuilder
import qualified Luna.Syntax.Builder.Star   as StarBuilder
import qualified Luna.Syntax.Builder.Symbol as SymbolBuilder
import           Luna.Syntax.Repr.Graph     (Edge (Edge), Node, Ref (Ref), TracksSuccs)
import qualified Luna.Syntax.Repr.Graph     as G
import qualified Luna.Syntax.AST.Typed      as Typed
import           Luna.Syntax.AST.Typed      (HasType)

type NodeType a    = ( Coated a
                     , Uncoated a ~ (Draft (Ref Edge))
                     , HasType a (Ref Edge)
                     , TracksSuccs a
                     )

type BuilderType m a = ( NodeType a
                       , StarBuilder.MonadStarBuilder (Maybe (Ref Node)) m
                       , NodeBuilder.MonadNodeBuilder (Ref Node) m
                       , BuilderMonad (Graph a DoubleArc) m
                       , MonadFix m
                       , CoatConstructor m a
                       , Destructor m (Ref Node)
                       )

pre :: BuilderType m a => Ref Node -> m [Ref Node]
pre ref = do
    node <- B.readRef ref
    mapM (B.follow) $ inputs $ uncoat node

createConsInt :: BuilderType m a => m (Ref Node)
createConsInt = do
    nameInt <- B._string "Int"
    B.cons nameInt

createConsString :: BuilderType m a => m (Ref Node)
createConsString = do
    nameString <- B._string "String"
    B.cons nameString

assignLiteralTypes :: BuilderType m a => Ref Node -> m ()
assignLiteralTypes ref = do
    consIntTpe    <- createConsInt
    consStringTpe <- createConsString
    assignLiteralTypesWithTypes consIntTpe consStringTpe ref

assignLiteralType :: BuilderType m a => Ref Node -> Ref Node -> m ()
assignLiteralType ref tpe = do
    node <- B.readRef ref
    tnodeRef <- B.follow $ node ^. Typed.tp
    tnode <- B.readRef tnodeRef
    case' (uncoat tnode) $ do
        match $ \Star -> do
            destruct tnodeRef
            void $ B.reconnect ref Typed.tp tpe
        match $ \ANY -> return ()

assignLiteralTypesWithTypes :: BuilderType m a => Ref Node -> Ref Node -> Ref Node -> m ()
assignLiteralTypesWithTypes consIntTpe consStringTpe ref = do
    node <- B.readRef ref
    -- B.writeRef ref (node & label . Label.checked .~ True)
    case' (uncoat node) $ do
        match $ \(Val val) -> do
            case' val $ match $ \lit -> assignLiteralType ref $ case lit of
                Lit.Int    _ -> consIntTpe
                Lit.String _ -> consStringTpe
        match $ \ANY -> return ()
    mapM_ (assignLiteralTypesWithTypes consIntTpe consStringTpe) =<< pre ref

