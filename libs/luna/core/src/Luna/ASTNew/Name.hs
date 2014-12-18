---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Luna.ASTNew.Name where

import           Flowbox.Prelude

import           GHC.Generics
import           Data.String             (IsString, fromString)

import qualified Luna.ASTNew.Name.Assert as Assert
import qualified Luna.ASTNew.Name.Path   as NamePath
import           Luna.ASTNew.Name.Path   (NamePath(NamePath))
import           Luna.ASTNew.Name.Hash   (Hashable, hash)

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

unsafeConvert :: (Wrapper m, Wrapper n) => m a -> n a
unsafeConvert = rewrap

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Name a = V  (VName  a)
            | T  (TName  a)
            | C  (CName  a)
            | TV (TVName a)
            deriving (Show, Eq, Ord, Generic, Read, Functor)

data NameBase a = VarName  (VName a)
                | TypeName (TName a)
                deriving (Show, Eq, Ord, Generic, Read, Functor)

newtype VName  a = VName  a deriving (Show, Eq, Ord, Generic, Read, Functor)
newtype TName  a = TName  a deriving (Show, Eq, Ord, Generic, Read, Functor)
newtype CName  a = CName  a deriving (Show, Eq, Ord, Generic, Read, Functor)
newtype TVName a = TVName a deriving (Show, Eq, Ord, Generic, Read, Functor)

type VNameP    = VName  NamePath
type TNameP    = TName  NamePath
type CNameP    = CName  NamePath
type TVNameP   = TVName NamePath
type NameBaseP = NameBase NamePath

vname  (Assert.isVName  -> s) = VName  s
tname  (Assert.isTName  -> s) = TName  s
cname  (Assert.isCName  -> s) = CName  s
tvname (Assert.isTVName -> s) = TVName s


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance IsString (VName  NamePath) where fromString = vname  . fromString
instance IsString (TName  NamePath) where fromString = tname  . fromString
instance IsString (CName  NamePath) where fromString = cname  . fromString
instance IsString (TVName NamePath) where fromString = tvname . fromString

instance IsString (VName  String)   where fromString = vname 
instance IsString (TName  String)   where fromString = tname 
instance IsString (CName  String)   where fromString = cname 
instance IsString (TVName String)   where fromString = tvname

instance ToString a => ToString (VName  a) where toString = toString . unwrap
instance ToString a => ToString (TName  a) where toString = toString . unwrap
instance ToString a => ToString (CName  a) where toString = toString . unwrap
instance ToString a => ToString (TVName a) where toString = toString . unwrap

instance FromText (VName  NamePath) where fromText = vname  . fromText
instance FromText (TName  NamePath) where fromText = tname  . fromText
instance FromText (CName  NamePath) where fromText = cname  . fromText
instance FromText (TVName NamePath) where fromText = tvname . fromText

instance ToText a => ToText (VName  a) where toText = toText . unwrap
instance ToText a => ToText (TName  a) where toText = toText . unwrap
instance ToText a => ToText (CName  a) where toText = toText . unwrap
instance ToText a => ToText (TVName a) where toText = toText . unwrap



instance Wrapper VName  where wrap             = VName
                              unwrap (VName a) = a
instance Wrapper TName  where wrap             = TName
                              unwrap (TName a) = a
instance Wrapper CName  where wrap             = CName
                              unwrap (CName a) = a
instance Wrapper TVName where wrap             = TVName
                              unwrap (TVName a) = a


-- only possible conversions
instance Convertible (VName  a) (TVName a) where convert = rewrap
instance Convertible (TVName a) (VName  a) where convert = rewrap
instance Convertible (CName  a) (TName  a) where convert = rewrap
instance Convertible (TName  a) (CName  a) where convert = rewrap

instance Convertible (VName  a) (VName  a) where convert = rewrap
instance Convertible (TName  a) (TName  a) where convert = rewrap
instance Convertible (CName  a) (CName  a) where convert = rewrap
instance Convertible (TVName a) (TVName a) where convert = rewrap

instance Hashable a b => Hashable (VName  a) b where hash = hash . unwrap
instance Hashable a b => Hashable (TName  a) b where hash = hash . unwrap
instance Hashable a b => Hashable (CName  a) b where hash = hash . unwrap
instance Hashable a b => Hashable (TVName a) b where hash = hash . unwrap