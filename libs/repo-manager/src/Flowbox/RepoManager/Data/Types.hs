---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.RepoManager.Data.Types where

import           Flowbox.Prelude
import qualified Data.List.Split as Split
import qualified System.FilePath as FilePath

data QualifiedPackageName = QualifiedPackageName { _name     :: String
                                                 , _category :: [String]
                                                 } deriving Eq

instance Show QualifiedPackageName where
    show qualpkgname = FilePath.joinPath (_category qualpkgname ++ [_name qualpkgname])

instance Ord QualifiedPackageName where
    compare q1 q2 = compare (_category q1) (_category q2) ++ compare (_name q1) (_name q2)

makeLenses ''QualifiedPackageName

makeQualified :: String -> Maybe QualifiedPackageName
makeQualified str = if validName then Just (QualifiedPackageName n c) else Nothing
    where validName = length splitted > 1
          splitted  = Split.splitOn "/" str
          n = last splitted
          c = init splitted