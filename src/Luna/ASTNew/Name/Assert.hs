---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Luna.ASTNew.Name.Assert where

import Flowbox.Prelude

import Control.Exception.Assert (byPred, assert)
import Data.Char.Class          (LetterCase, isLower, isUpper)
import Luna.ASTNew.Name.Path    (NamePath(NamePath))

----------------------------------------------------------------------
-- Wrappers
----------------------------------------------------------------------

newtype VarName = VarName String deriving (Show)


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
    isLower (VarName name@(start:_)) = isLower name || start == '_'
    isUpper (VarName name)           = isUpper name


instance OpName Char where
    isOpName = (`elem` opChars)

instance OpName String where
    isOpName = null . filter (not . isOpName)

instance OpName NamePath where
	isOpName (NamePath base _) = isOpName base