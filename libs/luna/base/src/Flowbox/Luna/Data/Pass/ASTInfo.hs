---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}


module Flowbox.Luna.Data.Pass.ASTInfo where

import qualified Flowbox.Luna.Data.AST.Common as AST
import           Flowbox.Prelude              hiding (id)


data ASTInfo = ASTInfo { _lastID :: AST.ID } deriving (Show)

makeLenses (''ASTInfo)


incID :: ASTInfo -> ASTInfo
incID = lastID %~ (+1)


mk :: AST.ID -> ASTInfo
mk = ASTInfo


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Default ASTInfo where
    def = ASTInfo 0
