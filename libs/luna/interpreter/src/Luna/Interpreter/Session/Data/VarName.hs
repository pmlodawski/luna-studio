---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.Session.Data.VarName where

import qualified Data.List as List

import           Flowbox.Prelude
import qualified Luna.Interpreter.Session.Data.CallPoint     as CallPoint
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.Hash          (Hash)
import qualified Luna.Lib.Lib                                as Library



data VarName = VarName { _callPointPath :: CallPointPath
                       , _hash          :: [Hash]
                       } deriving (Show, Eq, Ord)


makeLenses ''VarName


instance Default VarName where
    def = VarName def def


toString :: VarName -> String
toString varName = concatMap gen (varName ^. callPointPath) ++ hashStr (varName ^. hash) where
    gen callPoint = "_" ++ show (abs $ Library.toInt (callPoint ^. CallPoint.libraryID))
                 ++ "_" ++ show (abs (callPoint ^. CallPoint.nodeID))
    hashStr [] = "_nohash"
    hashStr h  = '_' : List.intercalate "_" (map show h)

