{-# LANGUAGE TemplateHaskell #-}

module Luna.Typechecker.Data.Typo where


import Control.Lens

import Data.Default

import Luna.Syntax.Enum     (ID)

import Luna.Typechecker.Data.TypeScheme



newtype Typo = Typo { fromTypo :: [(ID,TypeScheme)] }

makePrisms ''Typo


instance Default Typo where
  def = Typo []


init_typo :: [Typo]
init_typo = [def]