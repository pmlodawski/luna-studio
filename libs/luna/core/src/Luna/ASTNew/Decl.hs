---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Luna.ASTNew.Decl where

import           GHC.Generics           (Generic)
import           Luna.ASTNew.Type       (RType)
import           Luna.ASTNew.Rec        (Rec)
import           Luna.ASTNew.Name       (VName, TName, CName, TVName)
import           Luna.ASTNew.Name.Multi (MultiName)
import           Luna.ASTNew.Arg        (Arg)


import qualified Prelude



data Decl f e
    = Data        { _tname   :: TName   , params    :: [TVName]  , _cons   :: [ConsDef f] , _defs   :: [RDecl f]                     }
    | Function    { _path    :: Path    , _fname    :: MultiName , _inputs :: [Arg f]     , _output :: RType f  , _body :: [e] }
    | Import      { _modPath :: Path    , _targets  :: [ImpTgt]                                                                      }
    | TypeAlias   { _dstType :: RType f , _srcType  :: RType f                                                                       }
    | TypeWrapper { _dstType :: RType f , _srcType  :: RType f                                                                       }
    deriving (Generic)


data ConsDef f = ConsDef { _consName :: CName, _fields :: [RField f] }
               deriving (Generic)

data Field f e = Field { _fType :: RType f, _fName :: Maybe VName, _fVal :: Maybe e }



type Path     = [TName]
type RDecl f = Rec f Decl
type RField f = Rec f Field