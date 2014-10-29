---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Luna.ASTNew.Type where

import Flowbox.Prelude
import GHC.Generics      (Generic)
import Luna.ASTNew.Name  (Name, VName, TName, CName, TVName)
import Luna.ASTNew.Label (Label)


data Type a
    = Function { _inputs   :: [LType a]  , _output  :: LType a   }
    | App      { _src      :: LType a    , _args    :: [LType a] }
    | Var      { _vname    :: VName                              }
    | Tuple    { _items    :: [LType a]                          }
    | List     { _item     :: LType a                            }
    | Con      { _segments :: [TName]                            }
    | Wildcard 
    deriving (Generic, Show)


type LType a = Label a (Type a)

