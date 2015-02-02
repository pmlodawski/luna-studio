{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Luna.Typechecker.Data.Subst where


import Control.Lens

import Data.Default
import qualified Data.Map as Map
import Data.Wrapper (Pack(..), Unpack(..))

import Luna.Typechecker.Data.TVar
import Luna.Typechecker.Data.Type


newtype Subst = Subst { fromSubst :: Map.Map TVar Type }


makePrisms ''Subst


instance Unpack Subst [(TVar, Type)] where unpack = toList
instance Pack [(TVar, Type)] Subst where pack = Subst . Map.fromList


instance Default Subst where
  def = Subst Map.empty


singleSubst :: TVar -> Type -> Subst
singleSubst x t = Subst $ Map.singleton x t


identitySubst :: Subst
identitySubst = Subst Map.empty


toList :: Subst -> [(TVar, Type)]
toList = Map.toList . fromSubst
