module Luna.Typechecker.Data.TypeScheme where


import Flowbox.Prelude

import Luna.Typechecker.Data.Constraint
import Luna.Typechecker.Data.Type
import Luna.Typechecker.Data.TVar



data TypeScheme = Mono Type
                | Poly [TVar] Constraint Type
                deriving (Show)