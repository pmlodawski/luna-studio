---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns              #-}

module Luna.Syntax.Name where

import Flowbox.Prelude

import           Data.Binary     (Binary)
import qualified Data.Char       as Char
import           Data.Char.Class (LetterCase, isLower, isUpper)
import           Data.String     (IsString, fromString)
import qualified Data.Text.Lazy  as Text
import           GHC.Generics

import qualified Luna.Syntax.Name.Assert as Assert
import           Luna.Syntax.Name.Hash   (Hashable, hash)
import           Luna.Syntax.Name.Path   (NamePath (NamePath))
import qualified Luna.Syntax.Name.Path   as NamePath

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

unsafeConvert :: (Wrapper m, Wrapper n) => m a -> n a
unsafeConvert = rewrap

mkNameBaseAccessor :: Text -> NameBaseP
mkNameBaseAccessor s  = if Text.null s || Char.isLower (Text.head s)
    then VarName  $ fromText s
    else TypeName $ fromText s

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

type VNameP    = VName    NamePath
type TNameP    = TName    NamePath
type CNameP    = CName    NamePath
type TVNameP   = TVName   NamePath
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

instance Binary a => Binary (VName  a)
instance Binary a => Binary (TName  a)
instance Binary a => Binary (CName  a)
instance Binary a => Binary (TVName a)
instance Binary a => Binary (NameBase a)

instance Wrap   VName  where wrap              = VName
instance Wrap   TName  where wrap              = TName
instance Wrap   CName  where wrap              = CName
instance Wrap   TVName where wrap              = TVName
instance Unwrap VName  where unwrap (VName  a) = a
instance Unwrap TName  where unwrap (TName  a) = a
instance Unwrap CName  where unwrap (CName  a) = a
instance Unwrap TVName where unwrap (TVName a) = a
instance Unwrap NameBase where
    unwrap = \case
        VarName  a -> unwrap a
        TypeName a -> unwrap a


-- only possible conversions
instance Convertible (VName  a) (TVName a) where safeConvert = Right . rewrap
instance Convertible (TVName a) (VName  a) where safeConvert = Right . rewrap
instance Convertible (CName  a) (TName  a) where safeConvert = Right . rewrap
instance Convertible (TName  a) (CName  a) where safeConvert = Right . rewrap

instance Convertible (VName  a) (VName  a) where safeConvert = Right . rewrap
instance Convertible (TName  a) (TName  a) where safeConvert = Right . rewrap
instance Convertible (CName  a) (CName  a) where safeConvert = Right . rewrap
instance Convertible (TVName a) (TVName a) where safeConvert = Right . rewrap

instance Convertible (VName a) (NameBase a) where safeConvert = Right . VarName
instance Convertible (TName a) (NameBase a) where safeConvert = Right . TypeName

instance Hashable a b => Hashable (VName    a) b where hash = hash . unwrap
instance Hashable a b => Hashable (TName    a) b where hash = hash . unwrap
instance Hashable a b => Hashable (CName    a) b where hash = hash . unwrap
instance Hashable a b => Hashable (TVName   a) b where hash = hash . unwrap
instance Hashable a b => Hashable (NameBase a) b where hash = hash . unwrap


instance (IsString a) => IsString (NameBase a) where
    fromString s = if isLower s then VarName  (VName $ fromString s)
                                else TypeName (TName $ fromString s)
