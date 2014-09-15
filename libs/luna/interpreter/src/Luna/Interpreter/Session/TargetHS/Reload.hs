---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.Session.TargetHS.Reload where

import           Data.Set (Set)
import qualified Data.Set as Set

import Flowbox.Prelude
import Luna.AST.Control.Crumb (Breadcrumbs)



data Reload = ReloadMany { _items :: Set ReloadItem }
            | ReloadLibrary
            deriving (Show)

data ReloadItem = ReloadItem { _breadcrumbs :: Breadcrumbs }
                deriving (Show, Ord, Eq)


instance Monoid Reload where
    mempty = ReloadMany def
    mappend (ReloadMany a) (ReloadMany b) = ReloadMany $ Set.union a b
    mappend _              _              = ReloadLibrary


makeLenses(''Reload)
makeLenses(''ReloadItem)


