---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Data.AST.AST (
    module Luna.Data.AST.AST,
    module Luna.Data.AST.Common
)where

import Luna.Data.AST.Common
import Luna.Data.AST.Expr   (Expr)
import Luna.Data.AST.Lit    (Lit)
import Luna.Data.AST.Module (Module)
import Luna.Data.AST.Pat    (Pat)
import Luna.Data.AST.Type   (Type)
import Flowbox.Prelude


data AST = Module { fromModule :: Module }
         | Expr   { fromExpr   :: Expr   }
         | Lit    { fromLit    :: Lit    }
         | Pat    { fromPat    :: Pat    }
         | Type   { fromType   :: Type   }
         deriving (Show)
