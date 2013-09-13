---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Luna.AST.Constant where

import qualified Flowbox.Prelude as Prelude
import           Flowbox.Prelude   (Show, Eq)
import           Flowbox.Generics.Deriving.QShow
import           GHC.Generics

data Constant = Integer Prelude.String
              | String  Prelude.String
              | Char    Prelude.Char
              deriving (Show, Eq, Generic)

instance QShow Constant