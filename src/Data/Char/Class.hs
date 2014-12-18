---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Char.Class where

import Flowbox.Prelude
import qualified Data.Char      as Char
import qualified Data.Text.Lazy as LText
import qualified Data.Text      as Text

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
    isLower = isLower . head
    isUpper = isUpper . head

instance LetterCase LText.Text where
    isLower = isLower . head . toList
    isUpper = isUpper . head . toList

instance LetterCase Text.Text where
    isLower = isLower . head . toList
    isUpper = isUpper . head . toList