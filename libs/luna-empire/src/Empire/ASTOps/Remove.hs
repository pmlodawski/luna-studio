{-# LANGUAGE FlexibleContexts #-}

module Empire.ASTOps.Remove where

import           Data.Attribute                (attr)
import           Data.Construction             (destruct)
import           Data.Layer.Cover              (uncover)
import           Prologue

import           Empire.ASTOp                  (ASTOp)
import           Empire.Data.AST               (NodeRef)
import qualified Luna.Syntax.AST.Typed         as Typed
import qualified Luna.Syntax.Model.Graph       as Graph
import           Luna.Syntax.Model.Layer.Class (Succs (..), Type (..))

removeNode :: ASTOp m => NodeRef -> m ()
removeNode ref = do
    node     <- Graph.read ref
    typeNode <- Graph.follow $ node ^. attr Type
    undefined typeNode
    void $ undefined ref

safeRemove :: ASTOp m => NodeRef -> m ()
safeRemove ref = do
    refCount <- getRefCount ref
    if refCount > 0
        then return ()
        else performSafeRemoval ref

getRefCount :: ASTOp m => NodeRef -> m Int
getRefCount ref = (length . view (attr Succs)) <$> Graph.read ref

performSafeRemoval :: ASTOp m => NodeRef -> m ()
performSafeRemoval ref = do
    node <- Graph.read ref
    toRemove <- mapM Graph.follow (Graph.inputs $ uncover node)
    removeNode ref
    mapM_ safeRemove toRemove

