---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Data.HAST.Lit where

import           Flowbox.Prelude hiding (Char, Integer, String)
import qualified Flowbox.Prelude as Prelude

data Lit = Char    Prelude.Char
         | String  Prelude.String
         | Int     Prelude.String
         | Integer Prelude.String
         | Float   Prelude.String
         deriving (Show, Eq)

