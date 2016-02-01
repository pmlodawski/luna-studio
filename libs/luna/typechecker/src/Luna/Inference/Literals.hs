{-# LANGUAGE GADTs #-}

module Luna.Inference.Literals
    ( assignLiteralTypes
    ) where

import           Data.Construction
import qualified Data.IntSet       as IntSet
import           Data.Layer.Coat   (CoatConstructor, Coated, Uncoated, uncoat)
import           Data.Variants     (ANY (..), case', match)
import           Prologue          hiding (cons, pre, succ)

import qualified Luna.Syntax.AST.Lit    as Lit
import           Luna.Syntax.AST.Term   (Draft, Star (..), Val (..))
import qualified Luna.Syntax.AST.Term   as Term
import qualified Luna.Syntax.Builder    as Builder
import           Luna.Syntax.Repr.Graph (DoubleArc, Edge, Graph, Node (..), Ref (..))
import qualified Luna.Syntax.Repr.Graph as Graph

import           Luna.Syntax.AST.Typed     (HasType)
import qualified Luna.Syntax.AST.Typed     as Typed
import           Luna.Syntax.Builder.Class (BuilderMonad)
import qualified Luna.Syntax.Builder.Node  as NodeBuilder
import qualified Luna.Syntax.Builder.Star  as StarBuilder
import           Luna.Syntax.Repr.Graph    (Edge (Edge), TracksSuccs)
import qualified Luna.Syntax.Repr.Graph    as G

type NodeType a = ( Coated a
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
    node <- Builder.readRef ref
    mapM (Builder.follow) $ Term.inputs $ uncoat node

--- TODO: Make possible to reuse (Empire.ASTOps.Remove)

removeNode :: BuilderType m a => Ref Node -> m ()
removeNode ref = do
    node     <- Builder.readRef ref
    typeNode <- Builder.follow $ node ^. Typed.tp
    destruct typeNode
    destruct ref

safeRemove :: BuilderType m a => Ref Node -> m ()
safeRemove ref = do
    refCount <- getRefCount ref
    when (refCount == 0) $ performSafeRemoval ref

getRefCount :: BuilderType m a => Ref Node -> m Int
getRefCount ref = (length . toList . view Graph.succs) <$> Builder.readRef ref

performSafeRemoval :: BuilderType m a => Ref Node -> m ()
performSafeRemoval ref = do
    node     <- Builder.readRef ref
    toRemove <- mapM Builder.follow (Term.inputs $ uncoat node)
    removeNode ref
    mapM_ safeRemove toRemove

---

createConsInt :: BuilderType m a => m (Ref Node)
createConsInt = do
    nameInt <- Builder._string "Int"
    Builder.cons nameInt

createConsString :: BuilderType m a => m (Ref Node)
createConsString = do
    nameString <- Builder._string "String"
    Builder.cons nameString

assignLiteralTypes :: BuilderType m a => Ref Node -> m ()
assignLiteralTypes ref = do
    consIntTpe    <- createConsInt
    consStringTpe <- createConsString
    assignLiteralTypesWithTypes consIntTpe consStringTpe ref
    safeRemove consIntTpe
    safeRemove consStringTpe

assignLiteralType :: BuilderType m a => Ref Node -> Ref Node -> m ()
assignLiteralType ref tpe = do
    node     <- Builder.readRef ref
    tnodeRef <- Builder.follow $ node ^. Typed.tp
    tnode    <- Builder.readRef tnodeRef
    case' (uncoat tnode) $ do
        match $ \Star -> do
            destruct tnodeRef
            void $ Builder.reconnect ref Typed.tp tpe
        match $ \ANY -> return ()

assignLiteralTypesWithTypes :: BuilderType m a => Ref Node -> Ref Node -> Ref Node -> m ()
assignLiteralTypesWithTypes consIntTpe consStringTpe ref = do
    node <- Builder.readRef ref
    case' (uncoat node) $ do
        match $ \(Val val) -> do
            case' val $ match $ \lit -> assignLiteralType ref $ case lit of
                Lit.Int    _ -> consIntTpe
                Lit.String _ -> consStringTpe
        match $ \ANY -> return ()
    mapM_ (assignLiteralTypesWithTypes consIntTpe consStringTpe) =<< pre ref
