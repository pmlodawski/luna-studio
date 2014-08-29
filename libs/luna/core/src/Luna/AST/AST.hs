---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.AST.AST (
    module Luna.AST.AST,
    module Luna.AST.Common
)where

import Flowbox.Prelude
import Luna.AST.Common
import Luna.AST.Expr   (Expr)
import Luna.AST.Lit    (Lit)
import Luna.AST.Module (Module)
import Luna.AST.Pat    (Pat)
import Luna.AST.Type   (Type)


data AST = Module { fromModule :: Module }
         | Expr   { fromExpr :: Expr   }
         | Lit    { fromLit :: Lit    }
         | Pat    { fromPat :: Pat    }
         | Type   { fromType :: Type   }
         deriving (Show)
