---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Luna.ASTNew.Name.Assert where

import qualified Luna.ASTNew.Name.Rules   as Rules
import           Control.Exception.Assert (byPred, assert)


----------------------------------------------------------------------
-- Assert functions
----------------------------------------------------------------------

isVName  n = byPred assert "Variable name"      Rules.isVName  n n
isTName  n = byPred assert "Type name"          Rules.isTName  n n
isCName  n = byPred assert "Constructor name"   Rules.isCName  n n
isTVName n = byPred assert "Type variable name" Rules.isTVName n n


