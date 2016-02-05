---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Luna.Target.HS.AST.DataType (
        module Luna.Target.HS.AST.DataType,
        module Luna.Target.HS.AST.Expr
)where

import           Luna.Target.HS.AST.Expr

empty :: Expr
empty = DataD "" [] [] []

