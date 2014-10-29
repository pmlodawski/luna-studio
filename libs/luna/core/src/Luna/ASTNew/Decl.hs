---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE OverlappingInstances #-}

module Luna.ASTNew.Decl where

import           Flowbox.Prelude        hiding (Cons, traverse)
import           GHC.Generics           (Generic)
import           Luna.ASTNew.Type       (RType)
import           Luna.ASTNew.Name       (Name, VName, TName, CName, TVName)
import           Luna.ASTNew.Name.Multi (MultiName)
import           Luna.ASTNew.Arg        (Arg)
import           Luna.ASTNew.Pat        (RPat)
import           Luna.ASTNew.Native     (Native)

import qualified Prelude

data Decl f e
    = Data        { _tname   :: TName   , params    :: [TVName]   , _cons    :: [RCons f e] , _defs   :: [RDecl f e]                    }
    | Function    { _path    :: Path    , _fname    :: MultiName  , _inputs  :: [Arg f e]   , _output :: Maybe (RType f) , _body :: [e] }
    | Import      { _modPath :: Path    , _rename   :: Maybe TName, _targets :: [ImpTgt]                                                }
    | TypeAlias   { _dstType :: RType f , _srcType  :: RType f                                                                          }
    | TypeWrapper { _dstType :: RType f , _srcType  :: RType f                                                                          }
    | Native      { _native  :: Native (RDecl f e)                                                                                      }
    -- | Foreign     Foreign
    deriving (Generic)

-- !!!
-- jezeli bedziemy mieli TemplateLuna to chcemy znac kolejnosc deklaracji
-- bo mozemy chciec dzielic plik po sparsowaniu

--data Foreign a = Foreign Target a

-- ???
-- moze pozwolic na
-- import Math: sin :: Double -> Double
-- wtedy foreigny sa zwyklym wrapperem na Decl

data Cons  f e = Cons   { _consName :: CName   , _fields :: [RField f e]                  } deriving (Generic)
data Field f e = Field  { _fType    :: RType f , _fName  :: Maybe VName, _fVal :: Maybe e } deriving (Generic)
-- FIXME[wd]: przeniesc w inne miejsce
data ImpTgt    = ImpVar  { _vName  :: VName   , _vRename :: Maybe VName }
               | ImpType { _tName  :: TName   , _tRename :: Maybe TName }
               | Wildcard deriving (Show, Eq, Generic, Read)

type Path       = [TName]
type RCons  f e = f (Cons f e)
type RDecl  f e = f (Decl f e)
type RField f e = f (Field f e)

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

deriving instance (Show (RCons f e), Show (RDecl f e), Show (RPat f), Show (RType f), Show (Native (RDecl f e)), Show e) => Show (Decl f e)
deriving instance (Show (f (Field f e)))   => Show (Cons f e)
deriving instance (Show (RType f), Show e) => Show (Field f e)

