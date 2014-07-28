---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Data.VarName where

import           Data.Hash  (Hash)
import qualified Data.Hash  as Hash
import qualified Data.Maybe as Maybe

import qualified Flowbox.Interpreter.Session.Data.CallPoint     as CallPoint
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Flowbox.Prelude



type VarName = String


mk :: Maybe Hash -> CallPointPath -> VarName
mk mhash callPointPath = concatMap gen callPointPath ++ hash where
    gen callPoint = "_" ++ (show $ abs (callPoint ^. CallPoint.libraryID))
                 ++ "_" ++ (show $ abs (callPoint ^. CallPoint.nodeID))
    hash = '_' : Maybe.maybe "" (show . Hash.asWord64)  mhash
