{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Luna.Typechecker.Data (
    module TVarMod,
    module SubstMod,
    module TypoMod,
    module TypeSchemeMod,
    module PredicateMod,
    module TypeMod,
    module ConstraintMod,
    TypeMap, TypeSchemeMap
  ) where

import Luna.Typechecker.Data.TVar       as TVarMod
import Luna.Typechecker.Data.Subst      as SubstMod
import Luna.Typechecker.Data.Typo       as TypoMod
import Luna.Typechecker.Data.TypeScheme as TypeSchemeMod
import Luna.Typechecker.Data.Predicate  as PredicateMod
import Luna.Typechecker.Data.Type       as TypeMod
import Luna.Typechecker.Data.Constraint as ConstraintMod

import Data.IntMap.Strict   (IntMap)



type MapID a        = IntMap a
type TypeMap        = MapID Type
type TypeSchemeMap  = MapID TypeScheme
