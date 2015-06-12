---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE OverlappingInstances #-}

module Luna.Syntax.Decl where

import Flowbox.Prelude hiding (Cons, traverse)

import Data.Binary              (Binary)
import GHC.Generics             (Generic)
import Luna.Syntax.Foreign      (Foreign)
import Luna.Syntax.Label        (Label)
import Luna.Syntax.Label        (Label (Label))
import Luna.Syntax.Name         (CNameP, NameBaseP, TNameP, TVNameP, VNameP)
import Luna.Syntax.Name.Pattern (ArgPat)
import Luna.Syntax.Native       (Native)
import Luna.Syntax.Pat          (LPat)
import Luna.Syntax.Pragma       (Pragma)
import Luna.Syntax.Type         (LType)

import qualified Prelude

type FuncSig a e = ArgPat a e

type FuncOutput a = Maybe (LType a)

type DataParams = [TVNameP]

type ForeignCode = Text

noParams = []
noBody   = []
noFields = []

data Decl a e
    = Data      { _dataDecl :: DataDecl a e                  }
    | Func      { _funcDecl :: FuncDecl a e [e]              }
    | TpAls     { _dstType :: LType a , _srcType  :: LType a }
    | TpWrp     { _dstType :: LType a , _srcType  :: LType a }
    | Foreign   (Foreign (ForeignDecl a e))
    | Imp       Imp
    | Pragma    Pragma
    deriving (Show, Generic, Eq, Read)


data ProxyName a = ProxyName a (Maybe Text)



data FuncDecl a e body = FuncDecl { _funcDeclPath   :: Path
                                  , _funcDeclSig    :: FuncSig a e
                                  , _funcDeclOutput :: FuncOutput a
                                  , _funcDeclBody   :: body
                                  } deriving (Show, Generic, Eq, Read)
data DataDecl a e      = DataDecl { _dataDeclName   :: TNameP
                                  , _dataDeclParams :: DataParams
                                  , _dataDeclCons   :: [LCons a e]
                                  , _dataDeclDecls  :: [LDecl a e]
                                  } deriving (Show, Generic, Eq, Read)


data ForeignDecl a e
    = FData (DataDecl a e)
    | FFunc (FuncDecl a e ForeignCode)
    | FImp  Text
    deriving (Show, Generic, Eq, Read)


data Cons  a e = Cons   { _consName :: CNameP  , _fields :: [LField a e]                   } deriving (Show, Generic, Eq, Read)
data Field a e = Field  { _fType    :: LType a , _fName  :: Maybe VNameP, _fVal :: Maybe e } deriving (Show, Generic, Eq, Read)

singleData name = Data (DataDecl name noParams [Label 0 modCons] noBody)
    where modCons = Cons (convert name) noFields

-- === Imports ===

data Imp = ModImp  { _modPath :: Path , _modRename :: Maybe TNameP }
         | DeclImp { _modPath :: Path , _targets   :: [ImpTgt]     }
         deriving (Show, Generic, Eq, Read)

data ImpTgt = ImpVar   { _vName  :: VNameP   , _vRename :: Maybe VNameP }
            | ImpType  { _tName  :: TNameP   , _tRename :: Maybe TNameP }
            | Wildcard { _hiding :: [NameBaseP]                         }
            deriving (Show, Generic, Eq, Read)

type Path       = [TNameP]
type LCons  a e = Label a (Cons a e)
type LDecl  a e = Label a (Decl a e)
type LField a e = Label a (Field a e)


makeLenses ''Decl
makeLenses ''Imp
makeLenses ''ImpTgt
makeLenses ''DataDecl
makeLenses ''FuncDecl

-- === DataBuilder ===

data DataBuilder a e = DataBuilder { _decl    :: DataDecl a e
                                   , _dfields :: [LField a e]
                                   } deriving (Show, Generic, Eq, Read)
makeLenses ''DataBuilder

dataBuilder name params = DataBuilder (DataDecl name params [] []) []

addCons   c = (decl . dataDeclCons ) %~ (++ [c])
addDecl   d = (decl . dataDeclDecls) %~ (++ [d])
addField  f = dfields %~ (++ [f])
addFields f = dfields %~ (++ f)

instance (Binary a, Binary e) => Binary (Cons a e)
instance (Binary a, Binary e) => Binary (DataDecl a e)
instance (Binary a, Binary e) => Binary (Decl a e)
instance (Binary a, Binary e) => Binary (Field a e)
instance (Binary a, Binary e) => Binary (FuncDecl a e [e])
instance (Binary a, Binary e) => Binary (FuncDecl a e ForeignCode)
instance Binary Imp
instance Binary ImpTgt
instance (Binary a, Binary e) => Binary (ForeignDecl a e)
