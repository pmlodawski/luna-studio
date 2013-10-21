---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.HAST.Lit where

import qualified Flowbox.Prelude as Prelude
import           Flowbox.Prelude hiding (String, Char, Integer)

data Lit = Char    Prelude.Char
         | String  Prelude.String
         | Integer Prelude.String
         | Float   Prelude.String
         deriving (Show, Eq)

