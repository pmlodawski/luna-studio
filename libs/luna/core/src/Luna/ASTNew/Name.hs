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
import qualified Luna.ASTNew.Name.Path  as NamePath
import           Luna.ASTNew.Name.Path (NamePath(NamePath))
import           Luna.ASTNew.Name.Hash (Hashable, hash)

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class Convert a b where
    convert :: a -> b
    default convert :: (IsName a, IsName b) => a -> b
    convert = toName . fromName


class IsName n where
    toName   :: NamePath -> n
    fromName :: n        -> NamePath

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Name = V  VName
          | T  TName
          | C  CName
          | TV TVName
          deriving (Show, Eq, Ord, Generic, Read)

data NameBase = VarName  VName
              | TypeName TName
              deriving (Show, Eq, Ord, Generic, Read)

newtype VName  = VName  NamePath deriving (Show, Eq, Ord, Generic, Read)
newtype TName  = TName  NamePath deriving (Show, Eq, Ord, Generic, Read)
newtype CName  = CName  NamePath deriving (Show, Eq, Ord, Generic, Read)
newtype TVName = TVName NamePath deriving (Show, Eq, Ord, Generic, Read)

vname  (Assert.isVName  -> s) = VName  s
tname  (Assert.isTName  -> s) = TName  s
cname  (Assert.isCName  -> s) = CName  s
tvname (Assert.isTVName -> s) = TVName s


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance IsString VName  where fromString = vname  . NamePath.single
instance IsString TName  where fromString = tname  . NamePath.single
instance IsString CName  where fromString = cname  . NamePath.single
instance IsString TVName where fromString = tvname . NamePath.single

instance ToString VName  where toString (VName  n) = toString n
instance ToString TName  where toString (TName  n) = toString n
instance ToString CName  where toString (CName  n) = toString n
instance ToString TVName where toString (TVName n) = toString n

instance IsName VName where
    toName             = VName
    fromName (VName n) = n

instance IsName TName where
    toName             = TName
    fromName (TName n) = n

instance IsName CName where
    toName             = CName
    fromName (CName n) = n

instance IsName TVName where
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

instance Hashable VName where hash (VName name) = hash name