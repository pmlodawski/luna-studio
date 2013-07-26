---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.State.FuncState(
    FuncState(..),
    make
) where

import           Luna.Network.Graph.Graph          (Graph)
import           Luna.Codegen.Context            as Context
import           Luna.Codegen.Context              (Context)
import           Luna.Codegen.Mode               as Mode
import           Luna.Codegen.Mode                 (Mode)

data FuncState = FuncState {graph    :: Graph, 
                            mode     :: Mode, 
                            ctx      :: Context, 
                            lastctx  :: Context
                           } deriving (Show)

make :: Graph -> FuncState
make g = FuncState g Mode.Auto Context.Pure Context.Pure
