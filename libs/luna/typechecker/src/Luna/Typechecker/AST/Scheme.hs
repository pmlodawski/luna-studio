module Luna.Typechecker.AST.Scheme (
    Scheme(..), toScheme, quantify
  ) where


import Luna.Typechecker.HasKind             (HasKind(..))
import Luna.Typechecker.Substitutions       (Types(..))
import Luna.Typechecker.Typeclasses         (Qual(..))

import Luna.Typechecker.AST.Type            (Tyvar(..),Type(..))

import Luna.Typechecker.AST.Internal.Scheme (Scheme(..))

import Luna.Typechecker.Internal.Logger


quantify :: (Monad m) => [Tyvar] -> Qual Type -> TCLoggerT m Scheme
quantify vs qt = do ks <- mapM kind vs'
                    return (Forall ks (apply s qt))
  where vs' = [v | v <- tv qt, v `elem` vs]
        s   = zip vs' (map TGen [0..])
--quantify vs qt = Forall ks (apply s qt)
--  where vs' = [v | v <- tv qt, v `elem` vs]
--        ks  = map kind vs'
--        s   = zip vs' (map TGen [0..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)



