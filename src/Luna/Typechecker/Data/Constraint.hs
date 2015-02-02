module Luna.Typechecker.Data.Constraint where


import Flowbox.Prelude

import Luna.Typechecker.Data.TVar
import Luna.Typechecker.Data.Predicate



data Constraint = C [Predicate]
                | Proj [TVar] [Predicate]
                deriving (Show)


instance Monoid Constraint where
    mempty = C [TRUE]
    mappend (C p1) (C p2)               = C (p1 ++ p2)
    mappend (C p1) (Proj tvr p2)        = Proj tvr (p1 ++ p2)
    mappend (Proj tvr p1) (C p2)        = Proj tvr (p1 ++ p2)
    mappend (Proj tv1 p1) (Proj tv2 p2) = Proj (tv1 ++ tv2) (p1 ++ p2)


true_cons :: Constraint
true_cons = C [TRUE]