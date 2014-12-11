---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}

module Luna.ASTNew.Name.Rules where

import Flowbox.Prelude
import qualified Data.Char as Char
import           Luna.ASTNew.Name.Multi (MultiName(MultiName))

----------------------------------------------------------------------
-- Const rules
----------------------------------------------------------------------

opChars  = "!#$%&*+/<=>?\\^|-~"


----------------------------------------------------------------------
-- Rule checking
----------------------------------------------------------------------

isVName n = isLowerName n || isOpName n
isTName   = isUpperName
isCName   = isUpperName
isTVName  = isLowerName

isOpName (MultiName n _) = n `subsetOf` opChars

isLowerName (MultiName [] _)    = False
isLowerName (MultiName (c:_) _) = Char.isLower c || c == '_'

isUpperName (MultiName [] _)    = False
isUpperName (MultiName (c:_) _) = Char.isUpper c


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

xs `subsetOf` ys = null $ filter (not . (`elem` ys)) xs