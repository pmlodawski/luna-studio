---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Luna.Target.HS.AST.Builder.Utils where

import Flowbox.Prelude
import Luna.Target.HS.AST.Expr as Expr
import Luna.Target.HS.AST.Lit  as Lit

val = Expr.VarE "val"