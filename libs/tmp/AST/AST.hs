---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module Luna.DEP.AST.AST (
    module Luna.DEP.AST.AST,
    module X
) where

import Flowbox.Prelude
import Luna.DEP.AST.Common as X
import Luna.DEP.AST.Expr   (Expr)
import Luna.DEP.AST.Lit    (Lit)
import Luna.DEP.AST.Module (Module)
import Luna.DEP.AST.Pat    (Pat)
import Luna.DEP.AST.Prop   as X
import Luna.DEP.AST.Type   (Type)



data AST = Module { fromModule :: Module }
         | Expr   { fromExpr   :: Expr   }
         | Lit    { fromLit    :: Lit    }
         | Pat    { fromPat    :: Pat    }
         | Type   { fromType   :: Type   }
         deriving (Show, Eq, Read)


class ASTWrapper a b where
    astWrap :: a -> b


instance ASTWrapper Module AST where
    astWrap = Module

instance ASTWrapper Expr AST where
    astWrap = Expr

instance ASTWrapper Pat AST where
    astWrap = Pat



