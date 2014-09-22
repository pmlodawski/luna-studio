---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE GADTs                     #-}

module Luna.Data.Config where

import Flowbox.Prelude
import qualified Luna.Pragma.Pragma as Pragma
import Data.TypeLevel.Set (InsertClass)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Config a = Config { _pragmaSet :: Pragma.PragmaSet a } deriving (Show)

makeLenses ''Config


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

registerPragma p conf = conf & pragmaSet %~ Pragma.register p

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance a~() => Default (Config a) where
    def = Config def