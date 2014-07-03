---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.Data.DefPoint where

import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs (Breadcrumbs)
import qualified Flowbox.Luna.Lib.Library                as Library
import           Flowbox.Prelude



data DefPoint = DefPoint { _libraryID   :: Library.ID
                         , _breadcrumbs :: Breadcrumbs
                         } deriving (Show, Ord, Eq)

makeLenses(''DefPoint)
