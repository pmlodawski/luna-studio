module Flowbox.Luna.Typechecker.Internal.AST.AST where

import Flowbox.Luna.Typechecker.Internal.AST.Expr   (Expr)
import Flowbox.Luna.Typechecker.Internal.AST.Lit    (Lit)
import Flowbox.Luna.Typechecker.Internal.AST.Module (Module)
import Flowbox.Luna.Typechecker.Internal.AST.Pat    (Pat)
import Flowbox.Luna.Typechecker.Internal.AST.Type   (Type)


-- # data AST = Expr   { fromExpr   :: Expr   }
-- #          | Lit    { fromLit    :: Lit    }
-- #          | Module { fromModule :: Module }
-- #          | Pat    { fromPat    :: Pat    }
-- #          | Type   { fromType   :: Type   }
-- #          deriving (Show)