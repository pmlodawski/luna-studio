---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Luna.ASTNew.Pat where

import GHC.Generics     (Generic)

import Flowbox.Prelude
import Luna.ASTNew.Type (Type)
import Luna.ASTNew.Lit  (Lit)
import Luna.ASTNew.Name (VName, CName)


--data Pat p t l
--    = App         { _src   :: p   , _args :: [p] }
--    | Typed       { _pat   :: p   , _cls  :: t   }
--    | Grouped     { _pat   :: p                  }
--    | Lit         { _lit   :: l                  }
--    | Tuple       { _items :: [p]                }
--    | Con         { _cname  :: CName             }
--    | Var         { _vname  :: VName             }
--    | Wildcard 
--    | RecWildcard
--    deriving (Show, Eq, Generic, Read)



type R f a = f (a f)

type RPat f = R f Pat

data Pat f 
    = App         { _src   :: R f Pat , _args :: [R f Pat] }
    | Typed       { _pat   :: R f Pat , _cls  :: R f Type  }
    | Grouped     { _pat   :: R f Pat                      }
    | Lit         { _lit   :: f Lit                        }
    | Tuple       { _items :: [R f Pat ]                   }
    | Con         { _cname  :: CName                       }
    | Var         { _vname  :: VName                       }
    | Wildcard 
    | RecWildcard
    deriving (Generic)