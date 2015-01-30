{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Luna.Typechecker.Data (
    TVar(..), tvarNum,
    Subst(..), substRaw,
    Typo,
    Type(..), Predicate(..), Constraint(..), TypeScheme(..),
    TypeMap, TypeSchemeMap,
    MapID,
    init_typo, true_cons
  ) where


import Control.Lens

import Data.Default
import Data.Monoid
import Data.IntMap.Strict   (IntMap)
import Data.Wrapper         (Pack(..), Unpack(..))

import Luna.Syntax.Enum     (ID)



type MapID a        = IntMap a
type TypeMap        = MapID Type
type TypeSchemeMap  = MapID TypeScheme



newtype TVar  = TVar { fromTVar :: Int }
              deriving (Eq)

instance Unpack TVar Int  where unpack = fromTVar
instance Pack   Int TVar  where pack   = TVar

instance Show   TVar      where show (TVar x) = show x

tvarNum :: Iso' TVar Int
tvarNum = iso fromTVar TVar



newtype Subst = Subst { fromSubst :: [(TVar, Type)] }

instance Unpack Subst [(TVar, Type)] where unpack = fromSubst
instance Pack   [(TVar, Type)] Subst where pack   = Subst

substRaw :: Iso' Subst [(TVar, Type)]
substRaw = iso fromSubst Subst

instance Default Subst where
  def = Subst []


type Typo          = [(ID,TypeScheme)]


data Type = TV TVar
          | Type `Fun` Type
          deriving (Show,Eq)


data Predicate  = TRUE
                | Type `Subsume` Type
                deriving (Show,Eq)


data Constraint = C [Predicate]
                | Proj [TVar] [Predicate]
                deriving (Show)


data TypeScheme = Mono Type
                | Poly [TVar] Constraint Type
                deriving (Show)


empty_typo :: Typo
empty_typo = []


init_typo :: [Typo]
init_typo = [empty_typo]


true_cons :: Constraint
true_cons = C [TRUE]


instance Monoid Constraint where
  mempty = C [TRUE]
  mappend (C p1) (C p2)               = C (p1 ++ p2)
  mappend (C p1) (Proj tvr p2)        = Proj tvr (p1 ++ p2)
  mappend (Proj tvr p1) (C p2)        = Proj tvr (p1 ++ p2)
  mappend (Proj tv1 p1) (Proj tv2 p2) = Proj (tv1 ++ tv2) (p1 ++ p2)
