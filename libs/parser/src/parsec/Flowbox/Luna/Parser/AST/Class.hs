---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.Luna.Parser.AST.Class where

import           Flowbox.Luna.Parser.AST.AST 
import qualified Flowbox.Luna.Parser.AST.Type     as Type
import           Flowbox.Luna.Parser.AST.Type       (Type)


mk :: String -> [String] -> Expr
mk name' params' =Class (Type.Class name' params') [] []


parseBody :: Expr -> Expr -> Expr
parseBody expr cls' = case expr of
    Function {} -> addMethod expr cls'
    _           -> cls' -- error $ "class parse body error " ++ show expr


addMethod :: Expr -> Expr -> Expr
addMethod method cls' = cls' { methods = method : methods cls' }


addField :: Expr -> Expr -> Expr
addField field cls' = cls' { fields = field : fields cls' }