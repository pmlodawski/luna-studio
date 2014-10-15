module Luna.Typechecker.TypeEnv (
    TypeEnv,
    mkTypeEnv, getTypeEnv, expandTypeEnv,
  ) where

import Luna.Typechecker.Assumptions
import Luna.Typechecker.IDs
import Luna.Typechecker.Substitution
import Luna.Typechecker.Type


newtype TypeEnv = TypeEnv Assumptions
                deriving (Show)

instance Types TypeEnv where
  apply s (TypeEnv a) = TypeEnv $ apply s a
  ftv (TypeEnv a) = ftv a

mkTypeEnv :: TypeEnv
mkTypeEnv = TypeEnv mkAssumptions

getTypeEnv :: TypeEnv -> VarID -> Maybe Scheme
getTypeEnv (TypeEnv env) = searchAssumptions env

expandTypeEnv :: TypeEnv -> VarID -> Scheme -> TypeEnv
expandTypeEnv (TypeEnv a) v s = TypeEnv $ expandAssumptions a v s

