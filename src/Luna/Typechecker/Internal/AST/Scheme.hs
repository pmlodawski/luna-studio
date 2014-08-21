module Luna.Typechecker.Internal.AST.Scheme (Scheme(..), toScheme, quantify) where

import qualified Luna.Typechecker.Internal.AST.Kind         as Knd
import qualified Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Luna.Typechecker.Internal.HasKind          as HKd
import qualified Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl


-- TODO [kgdk] 20 sie 2014: przenieść Scheme.hs do AST/Scheme.hs
data Scheme = Forall [Knd.Kind] (Tcl.Qual Ty.Type)
            deriving (Eq,Show)


instance Sub.Types Scheme where
  apply s (Forall ks qt) = Forall ks (Sub.apply s qt)
  tv (Forall _ qt) = Sub.tv qt


quantify :: [Ty.Tyvar] -> Tcl.Qual Ty.Type -> Scheme
quantify vs qt = Forall ks (Sub.apply s qt)
  where vs' = [v | v <- Sub.tv qt, v `elem` vs]
        ks  = map HKd.kind vs'
        s   = zip vs' (map Ty.TGen [0..])

toScheme :: Ty.Type -> Scheme
toScheme t = Forall [] ([] Tcl.:=> t)



