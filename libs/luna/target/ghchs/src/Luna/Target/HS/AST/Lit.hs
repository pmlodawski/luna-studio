---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Target.HS.AST.Lit where

import           Flowbox.Prelude hiding (Char, Integer, String)
import qualified Flowbox.Prelude as Prelude

data Lit = Char    Prelude.Char
         | String  Text
         | Int     Text
         | Integer Text
         | Float   Text
         deriving (Show, Eq)

