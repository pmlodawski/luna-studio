---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}


module Luna.ASTNew.Name.Path where


import GHC.Generics (Generic)

import           Flowbox.Prelude
import qualified Data.Map              as Map
import           Data.Map              (Map)
import           Flowbox.Generics.Deriving.QShow
import           Data.String.Utils     (join)
import           Data.List             (intersperse)
import           Data.String           (IsString, fromString)
import           Luna.ASTNew.Name.Hash (Hashable, hash)
import           Data.Text.Lazy        (Text)

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data NamePath = NamePath { _base :: Text, _segments :: [Text] }
          deriving (Show, Eq, Generic, Read, Ord)

makeLenses ''NamePath
instance QShow (NamePath)



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

instance FromText NamePath where fromText = single

instance Hashable NamePath Text where
    hash (NamePath base segs) = hash base <> mjoin "_" (mempty : segs)

instance ToText NamePath where
    toText (NamePath base segs) = base <> mjoin " " (mempty : segs)

instance ToString NamePath where
	toString = toString . toText