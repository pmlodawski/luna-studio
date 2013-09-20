---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.Luna.Data.AST.Import where

import           Flowbox.Prelude              
import qualified Flowbox.Luna.Data.AST.Expr as Expr
import           Flowbox.Luna.Data.AST.Expr   (Expr)
import qualified Flowbox.Luna.Data.AST.Type as Type


--mk :: Int -> [String] -> Maybe String -> Expr
--mk id segments' mname = Expr.Import id segments' $ case mname of
--                            Just  n -> n
--                            Nothing -> last segments'



