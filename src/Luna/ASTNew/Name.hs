---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Luna.ASTNew.Name (
    module Luna.ASTNew.Name,
    module X
) where

import           Flowbox.Prelude

import           GHC.Generics
import           Data.String             (IsString, fromString)

import           Luna.ASTNew.Name.Rules  as X
import qualified Luna.ASTNew.Name.Assert as Assert

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class Convert a b where
    convert :: a -> b
    default convert :: (NameClass a, NameClass b) => a -> b
    convert = toName . fromName


class NameClass n where
    toName   :: String -> n
    fromName :: n      -> String

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Name = V  VName
          | T  TName
          | C  CName
          | TV TVName
          deriving (Show, Eq, Generic, Read)

data NameBase = VarName  VName
              | TypeName TName
              deriving (Show, Eq, Generic, Read)

newtype VName  = VName  String deriving (Show, Eq, Generic, Read)
newtype TName  = TName  String deriving (Show, Eq, Generic, Read)
newtype CName  = CName  String deriving (Show, Eq, Generic, Read)
newtype TVName = TVName String deriving (Show, Eq, Generic, Read)

vname  (Assert.isVName  -> s) = VName  s
tname  (Assert.isTName  -> s) = TName  s
cname  (Assert.isCName  -> s) = CName  s
tvname (Assert.isTVName -> s) = TVName s


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance IsString VName  where fromString = vname
instance IsString TName  where fromString = tname
instance IsString CName  where fromString = cname
instance IsString TVName where fromString = tvname

instance NameClass VName where
    toName             = VName
    fromName (VName n) = n

instance NameClass TName where
    toName             = TName
    fromName (TName n) = n

instance NameClass CName where
    toName             = CName
    fromName (CName n) = n

instance NameClass TVName where
    toName              = TVName
    fromName (TVName n) = n

instance Convert VName  TVName
instance Convert TVName VName
instance Convert CName  TName
instance Convert TName  CName

instance Convert VName  VName
instance Convert TName  TName
instance Convert CName  CName
instance Convert TVName TVName
