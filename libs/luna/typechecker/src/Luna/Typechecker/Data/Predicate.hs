module Luna.Typechecker.Data.Predicate where


import Flowbox.Prelude

import Luna.Typechecker.AlphaEquiv
import Luna.Typechecker.Data.Type



data Predicate  = TRUE
                | Type `Subsume` Type
                deriving (Show,Eq,Ord)


instance AlphaEquiv Predicate where
    equiv a b
      | isIdentity a && isIdentity b =
        return ()
      where
        isIdentity TRUE                     = True
        isIdentity (x `Subsume` y) | x == y = True
        isIdentity _                        = False
    equiv (p1 `Subsume` q1) (p2 `Subsume` q2) = equiv p1 p2 >> equiv q1 q2
    equiv _ _ = notAlphaEquivalent

    translateBtoA TRUE            = pure TRUE
    translateBtoA (p `Subsume` q) = Subsume <$> translateBtoA p <*> translateBtoA q