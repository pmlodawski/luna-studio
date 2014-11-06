---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Luna.ASTNew.Type where

import Flowbox.Prelude
import GHC.Generics     (Generic)
import Luna.ASTNew.Name (Name, VName, TName, CName, TVName)

type R f a = f (a f)

data Type f
    = Data     { _tname    :: TName      , _params  :: [TVName]  }
    | Module   { _tname    :: TName      , _path    :: [TName]   }
    | Function { _inputs   :: [RType f]  , _output  :: RType f   }
    | App      { _src      :: RType f    , _args    :: [RType f] }
    | Var      { _vname    :: VName                              }
    | Tuple    { _items    :: [RType f]                          }
    | List     { _item     :: RType f                            }
    | Con      { _segments :: [CName]                            }
    | Unknown 
    deriving (Generic)


type RType f = R f Type


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

deriving instance (Show (f (Type f))) => Show (Type f)