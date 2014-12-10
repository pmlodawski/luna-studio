---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}


module Luna.Data.ASTInfo where

import           Flowbox.Prelude hiding (id)
import qualified Luna.AST.Common as AST
import           Control.Monad.RWS (RWST, get, put)

data ASTInfo = ASTInfo { _lastID :: AST.ID } deriving (Show)

makeLenses (''ASTInfo)


incID :: ASTInfo -> ASTInfo
incID = lastID %~ (+1)


mk :: AST.ID -> ASTInfo
mk = ASTInfo


class ASTInfoClass m where
    getASTInfo :: m ASTInfo
    putASTInfo :: ASTInfo -> m ()


genID = do
    info <- getASTInfo
    putASTInfo $ incID info
    return $ info ^. lastID

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Default ASTInfo where
    def = ASTInfo 0


instance (Monoid w, Monad m) => ASTInfoClass (RWST r w ASTInfo m) where
    getASTInfo = get
    putASTInfo = put


