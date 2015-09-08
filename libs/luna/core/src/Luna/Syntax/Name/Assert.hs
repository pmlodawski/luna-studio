---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Luna.Syntax.Name.Assert where

import Flowbox.Prelude

import           Control.Exception.Assert (byPred, assert)
import           Data.Char.Class          (LetterCase, isLower, isUpper)
import           Luna.Syntax.Name.Path    (NamePath(NamePath))
import qualified Data.Text.Lazy           as Text

----------------------------------------------------------------------
-- Wrappers
----------------------------------------------------------------------

newtype VarName = VarName Text deriving (Show)


----------------------------------------------------------------------
-- TypeClasses
----------------------------------------------------------------------

class OpName a where
    isOpName :: a -> Bool


----------------------------------------------------------------------
-- Assert functions
----------------------------------------------------------------------

isVName  n = byPred assert "Variable name"      (\ n -> isLower n || isOpName n)  n n
isTName  n = byPred assert "Type name"          isUpper  n n
isCName  n = byPred assert "Constructor name"   isUpper  n n
isTVName n = byPred assert "Type variable name" isLower  n n

----------------------------------------------------------------------
-- Const rules
----------------------------------------------------------------------

opChars  = "!#$%&*+/<=>?\\^|-~"


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance LetterCase NamePath where
    isLower (NamePath base _) = isLower (VarName base)
    isUpper (NamePath base _) = isUpper base


instance LetterCase VarName where
    isUpper (VarName name) = isUpper name
    isLower (VarName name) = isLower name || start == '_'
    	where start = head $ toString name


instance OpName Char where
    isOpName = (`elem` opChars)

instance OpName String where
    isOpName = null . filter (not . isOpName)

instance OpName Text where
    isOpName = Text.null . Text.filter (not . isOpName)

instance OpName NamePath where
	isOpName (NamePath base _) = isOpName base