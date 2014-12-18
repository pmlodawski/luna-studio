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

import Flowbox.Prelude hiding (Cons, traverse)

import GHC.Generics              (Generic)
import Luna.ASTNew.Type          (LType)
import Luna.ASTNew.Name          (VNameP, TNameP, CNameP, TVNameP)
import Luna.ASTNew.Arg           (Arg)
import Luna.ASTNew.Native        (Native)
import Luna.ASTNew.Label         (Label)
import Luna.ASTNew.Name.Pattern  (NamePattern)
import Luna.ASTNew.Pat           (LPat)
import Luna.ASTNew.Name.Pattern2 (ArgPat)

import qualified Prelude

type FuncSignature a e = ArgPat (LPat a) e


data Decl a e
    = Data        { _tname   :: TNameP   , params    :: [TVNameP]          , _cons    :: [LCons a e]     , _defs   :: [LDecl a e]                    }
    | Function    { _path    :: Path    , _sig      :: FuncSignature a e , _output  :: Maybe (LType a) , _body :: [e]           }
    | Import      { _modPath :: Path    , _rename   :: Maybe TNameP       , _targets :: [ImpTgt]                                                }
    | TypeAlias   { _dstType :: LType a , _srcType  :: LType a                                                                           }
    | TypeWrapper { _dstType :: LType a , _srcType  :: LType a                                                                           }
    | Native      { _native  :: Native (LDecl a e)                                                                                       }
    -- | Foreign     Foreign
    deriving (Show, Eq, Generic, Read)

-- !!!
-- jezeli bedziemy mieli TemplateLuna to chcemy znac kolejnosc deklaracji
-- bo mozemy chciec dzielic plik po sparsowaniu

--data Foreign a = Foreign Target a

-- ???
-- moze pozwolic na
-- import Math: sin :: Double -> Double
-- wtedy foreigny sa zwyklym wrapperem na Decl

data Cons  a e = Cons   { _consName :: CNameP   , _fields :: [LField a e]                  } deriving (Show, Eq, Generic, Read)
data Field a e = Field  { _fType    :: LType a , _fName  :: Maybe VNameP, _fVal :: Maybe e } deriving (Show, Eq, Generic, Read)
-- FIXME[wd]: przeniesc w inne miejsce
data ImpTgt    = ImpVar  { _vName  :: VNameP   , _vRename :: Maybe VNameP }
               | ImpType { _tName  :: TNameP   , _tRename :: Maybe TNameP }
               | Wildcard deriving (Show, Eq, Generic, Read)

type Path       = [TNameP]
type LCons  a e = Label a (Cons a e)
type LDecl  a e = Label a (Decl a e)
type LField a e = Label a (Field a e)


makeLenses ''Decl