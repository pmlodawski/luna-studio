---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Luna.ASTNew.Decl where

import           Flowbox.Prelude
import           GHC.Generics           (Generic)
import           Luna.ASTNew.Type       (RType)
import           Luna.ASTNew.Rec        (Rec)
import           Luna.ASTNew.Name       (Name, VName, TName, CName, TVName)
import           Luna.ASTNew.Name.Multi (MultiName)
import           Luna.ASTNew.Arg        (Arg)


import qualified Prelude



data Decl f e
    = Data        { _tname   :: TName   , params    :: [TVName]  , _cons   :: [ConsDef f e] , _defs :: [RDecl f e]                }
    -- | Function    { _path    :: Path    , _fname    :: MultiName , _inputs :: [Arg f e]     , _output :: RType f   , _body :: [e] }
    | Import      { _modPath :: Path    , _targets  :: [ImpTgt]                                                                   }
    | TypeAlias   { _dstType :: RType f , _srcType  :: RType f                                                                    }
    | TypeWrapper { _dstType :: RType f , _srcType  :: RType f                                                                    }
    deriving (Generic)


data ConsDef f e = ConsDef { _consName :: CName, _fields :: [RField f e] }
               deriving (Generic)

data Field f e = Field { _fType :: RType f, _fName :: Maybe VName, _fVal :: Maybe e }

data ImpTgt = ImpTgt { _impName :: Name, _rename :: Maybe Name } deriving (Show, Eq, Generic, Read)


type Path     = [TName]
type RDecl f e = f (Decl f e)
type RField f e = f (Field f e)