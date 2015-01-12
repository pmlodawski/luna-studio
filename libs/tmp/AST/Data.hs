---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}

module Luna.AST.Data where

import           Flowbox.Prelude   hiding (cons)
import           Luna.AST.Common   (ID)
import           Luna.AST.Expr     
import qualified Luna.AST.Expr     as Expr
import qualified Luna.AST.Type     as Type
import           Luna.AST.Type     (Type)


--mk :: ID -> Type -> Expr -> Expr
mk cons id cls con = cons id cls [con] [] []




--closeDefinition :: Expr -> Expr
--closeDefinition d = nd where
--    dcons = d ^. cons
--    defc  = last dcons
--    ncons = if length dcons == 1
--                then [defc & name .~ (nd ^. (cls. Type.name))]
--                else init dcons
--    nd = d & cons .~ ncons