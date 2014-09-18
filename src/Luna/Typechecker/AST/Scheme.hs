module Luna.Typechecker.AST.Scheme (
    Scheme(..), toScheme, quantify
  ) where

import Luna.Typechecker.AST.Internal.Scheme (Scheme(..))

import Luna.Typechecker.AST.Type            (Tyvar(..),Type(..))

import Luna.Typechecker.HasKind             (HasKind(..))
import Luna.Typechecker.Substitutions       (Types(..))
import Luna.Typechecker.Typeclasses         (Qual(..))


quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where vs' = [v | v <- tv qt, v `elem` vs]
        ks  = map kind vs'
        s   = zip vs' (map TGen [0..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)



