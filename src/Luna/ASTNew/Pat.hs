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
import Luna.ASTNew.Type (RType)
import Luna.ASTNew.Lit  (Lit)
import Luna.ASTNew.Name (VName, CName)


type RPat f = f (Pat f)

data Pat f 
    = App         { _src   :: RPat f , _args :: [RPat f] }
    | Typed       { _pat   :: RPat f , _cls  :: RType f  }
    | Grouped     { _pat   :: RPat f                     }
    | Lit         { _lit   :: f Lit                      }
    | Tuple       { _items :: [RPat f ]                  }
    | Con         { _cname  :: CName                     }
    | Var         { _vname  :: VName                     }
    | Wildcard 
    | RecWildcard
    deriving (Generic)


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

deriving instance (Show (RPat f), Show (RType f), Show (f Lit)) => Show (Pat f)
