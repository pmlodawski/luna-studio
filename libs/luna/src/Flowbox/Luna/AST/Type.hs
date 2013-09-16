---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Luna.AST.Type where

import           Flowbox.Prelude  hiding(id)
import           Flowbox.Generics.Deriving.QShow
import           Flowbox.Luna.AST.Utils      (ID)
import           GHC.Generics

data Type = Unknown
          | Var    { id :: ID, name     :: String                       }
	      | Tuple  { id :: ID, items    :: [Type]                       }
	      | Class  { id :: ID, name     :: String , params  :: [String] }
          | Module { id :: ID, name     :: String                       }
	      | Lambda { id :: ID, inputs   :: [Type] , outputs :: [Type]   }
	      | Cons   { id :: ID, segments :: [String]                     }
	      | App    { id :: ID, src      :: Type   , args      :: [Type] }
	      -- | List
	      -- | Map
          deriving (Show, Eq, Generic)


instance QShow Type