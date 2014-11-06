---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Luna.ASTNew.Pat where

import GHC.Generics     (Generic)

import Flowbox.Prelude
import Luna.ASTNew.Type  (LType)
import Luna.ASTNew.Lit   (Lit)
import Luna.ASTNew.Name  (VName, CName)
import Luna.ASTNew.Label (Label)


type LPat a = Label a (Pat a)
type L      = Label

data Pat a 
    = App         { _src   :: LPat a    , _args :: [LPat a] }
    | Typed       { _pat   :: LPat a    , _cls  :: LType a  }
    | Grouped     { _pat   :: LPat a                        }
    | Lit         { _lit   :: L a Lit                       }
    | Tuple       { _items :: [LPat a ]                     }
    | Con         { _cname :: CName                         }
    | Var         { _vname :: VName                         }
    | Wildcard 
    | RecWildcard
    deriving (Show, Eq, Generic, Read)

