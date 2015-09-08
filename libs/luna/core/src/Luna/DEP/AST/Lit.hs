---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Luna.DEP.AST.Lit where

import Control.Lens
import Data.Binary  (Binary)
import GHC.Generics

import           Flowbox.Generics.Deriving.QShow
import           Flowbox.Prelude                 (Eq, Read, Show)
import qualified Flowbox.Prelude                 as Prelude
import           Luna.DEP.AST.Common             (ID)
import           Luna.DEP.AST.Lit.Number         (Number)



data Lit = Char    { _id :: ID, _char :: Prelude.Char   }
         | String  { _id :: ID, _str  :: Prelude.String }
         | Number  { _id :: ID, _num  :: Number         }
         deriving (Show, Eq, Generic, Read)

instance Binary Lit

instance QShow Lit
makeLenses ''Lit
