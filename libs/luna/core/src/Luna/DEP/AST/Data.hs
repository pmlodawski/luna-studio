---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.DEP.AST.Data where

import           Flowbox.Prelude     hiding (cons)
import           Luna.DEP.AST.Common (ID)
import           Luna.DEP.AST.Expr
import qualified Luna.DEP.AST.Expr   as Expr
import           Luna.DEP.AST.Type   (Type)
import qualified Luna.DEP.AST.Type   as Type


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
