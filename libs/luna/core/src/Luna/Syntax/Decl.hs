---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE OverlappingInstances #-}

module Luna.Syntax.Decl where

import Flowbox.Prelude hiding (Cons, traverse)

import GHC.Generics             (Generic)
import Luna.Syntax.Type         (LType)
import Luna.Syntax.Name         (VNameP, TNameP, CNameP, TVNameP)
import Luna.Syntax.Native       (Native)
import Luna.Syntax.Label        (Label)
import Luna.Syntax.Pat          (LPat)
import Luna.Syntax.Name.Pattern (ArgPat)
import Luna.Syntax.Foreign      (Foreign)
import Luna.Syntax.Label        (Label(Label))

import qualified Prelude

type FuncSig a e = ArgPat a e

type FuncOutput a = Maybe (LType a)

type DataParams = [TVNameP]

type ForeignCode = Text

noParams = []
noBody   = []
noFields = []

data Decl a e
    = Data   (DataDecl a e)
    | Func   (FuncDecl a e [e])
    | Imp    { _modPath :: Path    , _rename   :: Maybe TNameP , _targets :: [ImpTgt]                                 }
    | TpAls  { _dstType :: LType a , _srcType  :: LType a                                                             }
    | TpWrp  { _dstType :: LType a , _srcType  :: LType a                                                             }
    | Foreign (Foreign (ForeignDecl a e))
    deriving (Show, Eq, Generic, Read)


data FuncDecl a e body = FuncDecl Path (FuncSig a e) (FuncOutput a) body      deriving (Show, Eq, Generic, Read)
data DataDecl a e      = DataDecl TNameP DataParams [LCons a e] [LDecl a e]   deriving (Show, Eq, Generic, Read)

data ForeignDecl a e
    = FData (DataDecl a e)
    | FFunc (FuncDecl a e ForeignCode)
    deriving (Show, Eq, Generic, Read)

-- !!!
-- jezeli bedziemy mieli TemplateLuna to chcemy znac kolejnosc deklaracji
-- bo mozemy chciec dzielic plik po sparsowaniu



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




singleData name = Data (DataDecl name noParams [Label 0 modCons] noBody)
    where modCons = Cons (convert name) noFields