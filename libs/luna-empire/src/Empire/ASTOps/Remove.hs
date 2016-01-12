module Empire.ASTOps.Remove where

import           Prologue
import           Data.Construction      (destruct)
import           Data.Layer.Coat        (uncoat)

import           Empire.ASTOp           (ASTOp)
import qualified Luna.Syntax.Repr.Graph as Graph
import qualified Luna.Syntax.AST.Typed  as Typed
import qualified Luna.Syntax.AST.Term   as Term
import qualified Luna.Syntax.Builder    as Builder
import           Luna.Syntax.Repr.Graph (Ref, Node)

removeNode :: Ref Node -> ASTOp ()
removeNode ref = do
    node     <- Builder.readRef ref
    typeNode <- Builder.follow $ node ^. Typed.tp
    destruct typeNode
    destruct ref

safeRemove :: Ref Node -> ASTOp ()
safeRemove ref = do
    refCount <- getRefCount ref
    if refCount > 0
        then return ()
        else performSafeRemoval ref

getRefCount :: Ref Node -> ASTOp Int
getRefCount ref = (length . toList . view Graph.succs) <$> Builder.readRef ref

performSafeRemoval :: Ref Node -> ASTOp ()
performSafeRemoval ref = do
    node <- Builder.readRef ref
    toRemove <- mapM Builder.follow (Term.inputs $ uncoat node)
    removeNode ref
    mapM_ safeRemove toRemove

