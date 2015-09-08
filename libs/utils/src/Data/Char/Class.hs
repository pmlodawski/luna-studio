---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Char.Class where

import qualified Data.Char       as Char
import           Data.Maybe      (fromMaybe, listToMaybe)
import qualified Data.Text       as Text
import qualified Data.Text.Lazy  as LText
import           Flowbox.Prelude

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class LetterCase a where
    isLower :: a -> Bool
    isUpper :: a -> Bool


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance LetterCase Char where
    isLower = Char.isLower
    isUpper = Char.isUpper

instance LetterCase String where
    isLower = onHead isLower
    isUpper = onHead isUpper

instance LetterCase LText.Text where
    isLower = onHead isLower . toList
    isUpper = onHead isUpper . toList

instance LetterCase Text.Text where
    isLower = onHead isLower . toList
    isUpper = onHead isUpper . toList

onHead :: (Char -> Bool) -> String -> Bool
onHead f = fromMaybe False . fmap f . listToMaybe
