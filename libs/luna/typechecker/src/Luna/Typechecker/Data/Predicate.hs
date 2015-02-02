module Luna.Typechecker.Data.Predicate where


import Flowbox.Prelude

import Luna.Typechecker.Data.Type



data Predicate  = TRUE
                | Type `Subsume` Type
                deriving (Show,Eq)