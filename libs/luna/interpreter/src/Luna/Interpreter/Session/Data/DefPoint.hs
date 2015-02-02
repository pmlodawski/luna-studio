---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Data.DefPoint where

import           Flowbox.Prelude
import qualified Luna.Lib.Lib              as Library
import           Luna.Syntax.Control.Crumb (Breadcrumbs)



data DefPoint = DefPoint { _libraryID   :: Library.ID
                         , _breadcrumbs :: Breadcrumbs
                         } deriving (Show, Ord, Eq)

makeLenses ''DefPoint

