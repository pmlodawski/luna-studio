---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Pat where

import           Data.Binary           (Binary)

import           Flowbox.Prelude
import           Luna.Syntax.Label     (Label)
import           Luna.Syntax.Lit       (Lit)
import qualified Luna.Syntax.Name      as Name
import           Luna.Syntax.Name.Path (NamePath)
import           Luna.Syntax.Type      (LType)

type VName = Name.VName NamePath
type CName = Name.CName NamePath

type LPat a = Label a (Pat a)
type L      = Label

data Pat a
    = App         { _src   :: LPat a    , _args :: [LPat a] }
    | Typed       { _pat   :: LPat a    , _cls  :: LType a  }
    | Grouped     { _pat :: LPat a                        }
    | Lit         { _lit :: L a Lit                       }
    | Tuple       { _items :: [LPat a ]                     }
    | Con         { _cname :: CName                         }
    | Var         { _vname :: VName                         }
    | Wildcard
    | RecWildcard
    deriving (Show, Generic, Eq, Read)

instance Binary a => Binary (Pat a)
