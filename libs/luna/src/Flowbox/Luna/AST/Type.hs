---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.AST.Type where

import           Flowbox.Prelude   

data Type = Unknown
	      | Type   { name   :: String                       }
	      | Tuple  { items  :: [Type]                       }
	      | Class  { name   :: String , params  :: [String] }
          | Module { name   :: String                       }
	      | Lambda { inputs :: Type   , outputs :: Type     }
          deriving (Show, Eq)