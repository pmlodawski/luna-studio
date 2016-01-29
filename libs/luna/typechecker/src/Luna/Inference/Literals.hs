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

import           Luna.Syntax.AST.Term       (Draft, Star(..), Val(..))
import qualified Luna.Syntax.AST.Term       as Term
import qualified Luna.Syntax.AST.Lit        as Lit
import qualified Luna.Syntax.Builder        as Builder
import qualified Luna.Syntax.Repr.Graph     as Graph
import           Luna.Syntax.Repr.Graph     (Graph, Ref(..), Node(..), Edge, DoubleArc)

import           Luna.Syntax.Builder.Class  (BuilderMonad)
import qualified Luna.Syntax.Builder.Node   as NodeBuilder
import qualified Luna.Syntax.Builder.Star   as StarBuilder
import qualified Luna.Syntax.Builder.Symbol as SymbolBuilder
import           Luna.Syntax.Repr.Graph     (Edge (Edge), Node, Ref (Ref), TracksSuccs)
import qualified Luna.Syntax.Repr.Graph     as G
import qualified Luna.Syntax.AST.Typed      as Typed
import           Luna.Syntax.AST.Typed      (HasType)

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

succ :: BuilderType m a => Ref Node -> m [Ref Node]
succ ref = do
    node <- Builder.readRef ref
    mapM (Builder.unfollow . Ref . Edge) $ IntSet.toList $ node ^. G.succs

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
    assignedIntTpe    <- succ consIntTpe
    assignedStringTpe <- succ consStringTpe
    when (null assignedIntTpe)    $ safeRemove consIntTpe
    when (null assignedStringTpe) $ safeRemove consStringTpe

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
    -- B.writeRef ref (node & label . Label.checked .~ True)
    case' (uncoat node) $ do
        match $ \(Val val) -> do
            case' val $ match $ \lit -> assignLiteralType ref $ case lit of
                Lit.Int    _ -> consIntTpe
                Lit.String _ -> consStringTpe
        match $ \ANY -> return ()
    mapM_ (assignLiteralTypesWithTypes consIntTpe consStringTpe) =<< pre ref

