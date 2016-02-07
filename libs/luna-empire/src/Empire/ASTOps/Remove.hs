{-# LANGUAGE FlexibleContexts #-}

module Empire.ASTOps.Remove where

import           Prologue                      hiding ((#))
import           Data.Construction             (destruct)
import           Data.Prop                     ((#))
import           Data.Layer.Cover              (uncover)

import           Empire.ASTOp                  (ASTOp)
import           Empire.Data.AST               (NodeRef)
import qualified Luna.Syntax.Builder as Builder
import           Luna.Syntax.Builder (Inputs (..), Type (..), Succs (..), target)

removeNode :: ASTOp m => NodeRef -> m ()
removeNode ref = do
    node     <- Builder.read ref
    typeNode <- Builder.follow target $ node # Type
    destruct typeNode
    void $ destruct ref

safeRemove :: ASTOp m => NodeRef -> m ()
safeRemove ref = do
    refCount <- getRefCount ref
    if refCount > 0
        then return ()
        else performSafeRemoval ref

getRefCount :: ASTOp m => NodeRef -> m Int
getRefCount ref = (length . (# Succs)) <$> Builder.read ref

performSafeRemoval :: ASTOp m => NodeRef -> m ()
performSafeRemoval ref = do
    node <- Builder.read ref
    toRemove <- mapM (Builder.follow target) $ uncover node # Inputs
    removeNode ref
    mapM_ safeRemove toRemove

