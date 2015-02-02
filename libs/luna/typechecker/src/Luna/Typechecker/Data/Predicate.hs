module Luna.Typechecker.Data.Predicate where

import Luna.Typechecker.Data.Type



data Predicate  = TRUE
                | Type `Subsume` Type
                deriving (Show,Eq)