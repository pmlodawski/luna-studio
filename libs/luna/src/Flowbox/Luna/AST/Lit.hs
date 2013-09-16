---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Luna.AST.Lit where

import qualified Flowbox.Prelude                 as Prelude
import           Flowbox.Prelude                   (Show, Eq)
import           Flowbox.Generics.Deriving.QShow   
import           Flowbox.Luna.AST.Utils            (ID)
import           GHC.Generics                      

data Lit = Char    { id :: ID, char :: Prelude.Char   }
         | String  { id :: ID, str  :: Prelude.String }
         | Integer { id :: ID, str  :: Prelude.String }
         deriving (Show, Eq, Generic)

instance QShow Lit

