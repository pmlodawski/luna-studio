---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Luna.ASTNew.Type where

import Flowbox.Prelude

import Luna.ASTNew.Label     (Label)
import Luna.ASTNew.Name.Path (NamePath)
import Luna.ASTNew.Name      (VNameP, TNameP)


data Type a
    = Function { _inputs   :: [LType a]  , _output  :: LType a   }
    | App      { _src      :: LType a    , _args    :: [LType a] }
    | Var      { _vname    :: VNameP                             }
    | Tuple    { _items    :: [LType a]                          }
    | List     { _item     :: LType a                            }
    | Con      { _segments :: [TNameP]                           }
    | Wildcard 
    deriving (Show, Eq, Ord, Generic, Read)


type LType a = Label a (Type a)

