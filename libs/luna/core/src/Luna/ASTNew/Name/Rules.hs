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
import           Luna.ASTNew.Name.Path (NamePath(NamePath))

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

isOpName (NamePath n _) = n `subsetOf` opChars

isLowerName (NamePath [] _)    = False
isLowerName (NamePath (c:_) _) = Char.isLower c || c == '_'

isUpperName (NamePath [] _)    = False
isUpperName (NamePath (c:_) _) = Char.isUpper c


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

xs `subsetOf` ys = null $ filter (not . (`elem` ys)) xs