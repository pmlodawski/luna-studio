---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Data.VarName where

import qualified Data.Maybe as Maybe

import           Flowbox.Prelude
import qualified Luna.Interpreter.Session.Data.CallPoint     as CallPoint
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.Hash          (Hash)
import qualified Luna.Lib.Lib                                as Library


type VarName = String


mk :: Maybe Hash -> CallPointPath -> VarName
mk mhash callPointPath = concatMap gen callPointPath ++ hash where
    gen callPoint = "_" ++ show (abs $ Library.toInt (callPoint ^. CallPoint.libraryID))
                 ++ "_" ++ show (abs (callPoint ^. CallPoint.nodeID))
    hash = '_' : Maybe.maybe "nohash" show  mhash
