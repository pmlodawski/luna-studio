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
-- Data types
----------------------------------------------------------------------

data Name = V  VName
          | T  TName
          | C  CName
          | TV TVName
          deriving (Show, Eq, Generic, Read)

newtype VName  = VName  String deriving (Show, Eq, Generic, Read)
newtype TName  = TName  String deriving (Show, Eq, Generic, Read)
newtype CName  = CName  String deriving (Show, Eq, Generic, Read)
newtype TVName = TVName String deriving (Show, Eq, Generic, Read)

vname  (Assert.isVName  -> s) = VName  s
tname  (Assert.isTName  -> s) = TName  s
cname  (Assert.isCName  -> s) = CName  s
tvname (Assert.isTVName -> s) = TVName s


data Named a n = Named   a n
               | Unnamed a  
               deriving (Show, Eq, Generic, Read)


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance IsString VName  where fromString = vname
instance IsString TName  where fromString = tname
instance IsString CName  where fromString = cname
instance IsString TVName where fromString = tvname