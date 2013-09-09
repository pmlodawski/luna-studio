---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.Luna.AST.Import where

import           Flowbox.Prelude         
import           Flowbox.Luna.AST.AST    
import qualified Flowbox.Luna.AST.Type as Type


mk :: [String] -> Maybe String -> Expr
mk segments' mname = Import segments' $ case mname of
                         Just  n -> n
                         Nothing -> last segments'



