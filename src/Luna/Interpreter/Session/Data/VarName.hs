---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Data.VarName where

import           Flowbox.Prelude
import qualified Luna.Interpreter.Session.Data.CallPoint     as CallPoint
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Luna.Lib.Lib                                as Library



type VarName = String


mk :: CallPointPath -> VarName
mk callPointPath = concatMap gen callPointPath where
    gen callPoint = "_" ++ show (abs $ Library.toInt (callPoint ^. CallPoint.libraryID))
                 ++ "_" ++ show (abs (callPoint ^. CallPoint.nodeID))
