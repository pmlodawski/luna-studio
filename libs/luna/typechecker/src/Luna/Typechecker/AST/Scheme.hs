module Luna.Typechecker.AST.Scheme (Scheme(..), toScheme, quantify) where

import           Luna.Typechecker.AST.Kind         (Kind(..))
import           Luna.Typechecker.AST.Type         (Tyvar(..),Type(..))

import           Luna.Typechecker.HasKind          (HasKind(..))
import           Luna.Typechecker.Substitutions    (Types(..))
import           Luna.Typechecker.Typeclasses      (Qual(..))

import           Text.Printf                                (printf)
import Control.DeepSeq

-- TODO [kgdk] 20 sie 2014: przenieść Scheme.hs do AST/Scheme.hs
data Scheme = Forall [Kind] (Qual Type)
            deriving (Eq)

instance NFData Scheme where
  rnf (Forall ks ql) = rnf ks `seq` rnf ql

instance Show Scheme where
  show (Forall [] ([] :=> t)) = printf "%s" (show t)
  show (Forall [] (ps :=> t)) = printf "%s => %s" (show ps) (show t)
  show (Forall ks ([] :=> t)) = printf "forall %s. %s" (show ks) (show t)
  show (Forall ks (ps :=> t)) = printf "forall %s. %s => %s" (show ks) (show ps) (show t)


instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall _ qt) = tv qt


quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where vs' = [v | v <- tv qt, v `elem` vs]
        ks  = map kind vs'
        s   = zip vs' (map TGen [0..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)



