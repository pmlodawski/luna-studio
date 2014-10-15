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
  apply = undefined
  tv = undefined

mkTypeEnv :: TypeEnv
mkTypeEnv = TypeEnv $ mkAssumptions

getTypeEnv :: TypeEnv -> VarID -> Maybe Scheme
getTypeEnv (TypeEnv env) varid = assumptionsSearch env varid

expandTypeEnv :: TypeEnv -> VarID -> Scheme -> TypeEnv
expandTypeEnv = undefined

