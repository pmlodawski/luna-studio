---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.Session.Data.KeyName where

import           Flowbox.Prelude
import qualified Luna.DEP.Lib.Lib                            as Library
import qualified Luna.Interpreter.Session.Data.CallPoint     as CallPoint
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)



data KeyName = KeyName { _callPointPath :: CallPointPath
                       } deriving (Show, Eq, Ord)


makeLenses ''KeyName


instance Default KeyName where
    def = KeyName def


toString :: KeyName -> String
toString keyName = concatMap gen (keyName ^. callPointPath) where
    gen callPoint = "_" ++ show (abs $ Library.toInt (callPoint ^. CallPoint.libraryID))
                 ++ "_" ++ show (abs (callPoint ^. CallPoint.nodeID))
