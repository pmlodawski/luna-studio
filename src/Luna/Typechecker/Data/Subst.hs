{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Luna.Typechecker.Data.Subst where


import Control.Lens

import Data.Default
import Data.Wrapper (Pack(..), Unpack(..))

import Luna.Typechecker.Data.TVar
import Luna.Typechecker.Data.Type



newtype Subst = Subst { fromSubst :: [(TVar, Type)] }

makePrisms ''Subst

instance Unpack Subst [(TVar, Type)] where unpack = fromSubst
instance Pack   [(TVar, Type)] Subst where pack   = Subst


instance Default Subst where
  def = Subst []
