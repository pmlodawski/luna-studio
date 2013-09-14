---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.Luna.AST.Class where

import           Flowbox.Prelude      
import qualified Flowbox.Luna.AST.Expr as Expr
import           Flowbox.Luna.AST.Expr   (Expr)
import qualified Flowbox.Luna.AST.Type as Type


--mk :: Int -> String -> [String] -> Expr
mk id cls = Expr.Class id cls [] [] []


--parseBody :: Expr -> Expr -> Expr
--parseBody expr cls' = case expr of
--    Function {} -> addMethod expr cls'
--    _           -> cls' -- error $ "class parse body error " ++ show expr


