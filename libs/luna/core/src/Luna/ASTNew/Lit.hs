---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Luna.ASTNew.Lit where

import           GHC.Generics
import           Luna.ASTNew.Lit.Number (Number)

import           Prelude (Show, Eq, Read)
import qualified Prelude


data Lit = Char    { _char :: Prelude.Char   }
         | String  { _str  :: Prelude.String }
         | Number  { _num  :: Number         }
         deriving (Show, Eq, Generic, Read)