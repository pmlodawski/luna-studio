---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Luna.AST.Type where

import           Flowbox.Prelude  
import           Flowbox.Generics.Deriving.QShow
import           GHC.Generics

data Type = Unknown
	      | Sig    { name   :: String                       }
	      | Tuple  { items  :: [Type]                       }
	      | Class  { name   :: String , params  :: [String] }
          | Module { name   :: String                       }
	      | Lambda { inputs :: Type   , outputs :: Type     }
	      | List
	      | Map
          deriving (Show, Eq, Generic)


instance QShow Type