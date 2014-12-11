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
import qualified Luna.ASTNew.Name.Multi  as MultiName
import           Luna.ASTNew.Name.Multi (MultiName(MultiName))

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class Convert a b where
    convert :: a -> b
    default convert :: (NameClass a, NameClass b) => a -> b
    convert = toName . fromName


class NameClass n where
    toName   :: MultiName -> n
    fromName :: n      -> MultiName

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

newtype VName  = VName  MultiName deriving (Show, Eq, Generic, Read)
newtype TName  = TName  MultiName deriving (Show, Eq, Generic, Read)
newtype CName  = CName  MultiName deriving (Show, Eq, Generic, Read)
newtype TVName = TVName MultiName deriving (Show, Eq, Generic, Read)

vname  (Assert.isVName  -> s) = VName  s
tname  (Assert.isTName  -> s) = TName  s
cname  (Assert.isCName  -> s) = CName  s
tvname (Assert.isTVName -> s) = TVName s


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance IsString VName  where fromString = vname  . MultiName.single
instance IsString TName  where fromString = tname  . MultiName.single
instance IsString CName  where fromString = cname  . MultiName.single
instance IsString TVName where fromString = tvname . MultiName.single

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
