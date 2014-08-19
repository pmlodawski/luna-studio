---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Luna.AST.Data where

import           Luna.AST.Common (ID)
import           Luna.AST.Expr   (Expr)
import qualified Luna.AST.Expr   as Expr
import           Luna.AST.Type   (Type)


mk :: ID -> Type -> Expr -> Expr
mk id cls con = Expr.Data id cls [con] [] []
