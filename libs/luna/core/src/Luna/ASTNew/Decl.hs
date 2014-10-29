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

import Flowbox.Prelude        hiding (Cons, traverse)
import GHC.Generics           (Generic)
import Luna.ASTNew.Type       (LType)
import Luna.ASTNew.Name       (Name, VName, TName, CName, TVName)
import Luna.ASTNew.Name.Multi (MultiName)
import Luna.ASTNew.Arg        (Arg)
import Luna.ASTNew.Native     (Native)
import Luna.ASTNew.Label      (Label)


import qualified Prelude

data Decl a e
    = Data        { _tname   :: TName   , params    :: [TVName]   , _cons    :: [LCons a e] , _defs   :: [LDecl a e]                    }
    | Function    { _path    :: Path    , _fname    :: MultiName  , _inputs  :: [Arg a e]   , _output :: Maybe (LType a) , _body :: [e] }
    | Import      { _modPath :: Path    , _rename   :: Maybe TName, _targets :: [ImpTgt]                                                }
    | TypeAlias   { _dstType :: LType a , _srcType  :: LType a                                                                          }
    | TypeWrapper { _dstType :: LType a , _srcType  :: LType a                                                                          }
    | Native      { _native  :: Native (LDecl a e)                                                                                      }
    -- | Foreign     Foreign
    deriving (Show, Generic)

-- !!!
-- jezeli bedziemy mieli TemplateLuna to chcemy znac kolejnosc deklaracji
-- bo mozemy chciec dzielic plik po sparsowaniu

--data Foreign a = Foreign Target a

-- ???
-- moze pozwolic na
-- import Math: sin :: Double -> Double
-- wtedy foreigny sa zwyklym wrapperem na Decl

data Cons  a e = Cons   { _consName :: CName   , _fields :: [RField a e]                  } deriving (Show, Generic)
data Field a e = Field  { _fType    :: LType a , _fName  :: Maybe VName, _fVal :: Maybe e } deriving (Show, Generic)
-- FIXME[wd]: przeniesc w inne miejsce
data ImpTgt    = ImpVar  { _vName  :: VName   , _vRename :: Maybe VName }
               | ImpType { _tName  :: TName   , _tRename :: Maybe TName }
               | Wildcard deriving (Show, Eq, Generic, Read)

type Path       = [TName]
type LCons  a e = Label a (Cons a e)
type LDecl  a e = Label a (Decl a e)
type RField a e = Label a (Field a e)


