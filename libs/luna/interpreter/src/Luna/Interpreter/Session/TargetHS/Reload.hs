---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.Session.TargetHS.Reload where

import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Flowbox.Prelude
import           Luna.AST.Control.Crumb (Breadcrumbs)
import qualified Luna.Lib.Lib           as Library



data Reload = NoReload
            | ReloadClasses { _items :: Set ReloadClass }
            | ReloadFunctions
            | ReloadLibrary
            deriving (Show)

data ReloadClass = ReloadClass { _breadcrumbs :: Breadcrumbs }
                deriving (Show, Ord, Eq)


type ReloadMap = Map Library.ID Reload


instance Monoid Reload where
    mempty = NoReload
    mappend  NoReload          a                = a
    mappend  a                 NoReload         = a
    mappend  ReloadFunctions   ReloadFunctions  = ReloadFunctions
    mappend (ReloadClasses a) (ReloadClasses b) = ReloadClasses $ Set.union a b
    mappend (ReloadClasses a)  _                = ReloadClasses a
    mappend  _                (ReloadClasses b) = ReloadClasses b
    mappend  _                 _                = ReloadLibrary


instance Default Reload where
    def = NoReload


makeLenses(''Reload)
makeLenses(''ReloadClass)


mkReloadClasses :: Breadcrumbs -> Reload
mkReloadClasses = ReloadClasses . Set.singleton . ReloadClass
