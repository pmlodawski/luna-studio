---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.DefPoint where

import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs (Breadcrumbs)
import qualified Flowbox.Luna.Lib.Library                as Library



type DefPoint = (Library.ID, Breadcrumbs)

