---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Interpreter.Session.Data.CallDataPath where

import           Flowbox.Interpreter.Session.Data.CallData      (CallData)
import qualified Flowbox.Interpreter.Session.Data.CallData      as CallData
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Flowbox.Interpreter.Session.Data.DefPoint      (DefPoint)
import           Flowbox.Luna.Data.Graph.Graph                  (Graph)
import           Flowbox.Luna.Data.Graph.Node                   (Node)
import qualified Flowbox.Luna.Data.Graph.Node                   as Node
import           Flowbox.Prelude


type CallDataPath  = [CallData]



toCallPointPath :: CallDataPath -> CallPointPath
toCallPointPath = map (view CallData.callPoint)


append :: CallDataPath -> DefPoint -> Graph -> (Node.ID, Node) -> CallDataPath
append callDataPath defPoint parentGraph n =
    callDataPath ++ [CallData.mk defPoint parentGraph n]
