---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Luna.Syntax.Name.Path where

import           Data.Binary                     (Binary)
import           Data.List                       (intersperse)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.String                     (IsString, fromString)
import           Data.String.Utils               (join)
import           Data.Text.Lazy                  (Text)

import           Flowbox.Generics.Deriving.QShow
import           Flowbox.Prelude
import           Luna.Syntax.Name.Hash           (Hashable, hash)

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data NamePath = NamePath { _base :: Text, _segments :: [Text] }
          deriving (Show, Eq, Generic, Read, Ord)


data QualPath = QualPath { _path :: [Text], _name :: Text }
              deriving (Show, Eq, Generic, Read, Ord)

makeLenses ''NamePath
makeLenses ''QualPath

instance QShow NamePath

instance Binary NamePath
instance Binary QualPath

instance Default QualPath where
    def = QualPath def def

toList :: NamePath -> [Text]
toList (NamePath b s) = b:s

single :: Text -> NamePath
single = flip NamePath []

multi :: Text -> [Text] -> NamePath
multi = NamePath


isSingle :: NamePath -> Bool
isSingle = null . view segments


isMulti :: NamePath -> Bool
isMulti = not . isSingle


--toStr :: NamePath -> Text
--toStr n = if isSingle n
--    then strRepr $ n^.base
--    else (strRepr $ n^.base) ++ (' ' : join " " (n^.segments))


--unified :: NamePath -> Text
--unified n = if isSingle n
--    then strRepr $ n^.base
--    else (strRepr $ n^.base) ++ ('_' : join "_" (n^.segments))


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance IsString NamePath where fromString = single . fromString
instance IsString QualPath where fromString = QualPath [] . fromString

instance FromText NamePath where fromText = single

instance Hashable NamePath Text where
    hash (NamePath base segs) = hash base <> mjoin "_" (mempty : segs)


instance Hashable QualPath Text where
    hash (QualPath path name) = mjoin "_" (path <> mempty) <> hash name

instance FromText QualPath where
    fromText = QualPath []

instance IsList QualPath where
    type (Item QualPath) = Text
    fromList lst                = QualPath (init lst) (last lst)
    toList (QualPath path name) = path ++ [name]

instance ToText NamePath where
    toText (NamePath base segs) = base <> mjoin " " (mempty : segs)

instance ToString NamePath where
    toString = toString . toText


