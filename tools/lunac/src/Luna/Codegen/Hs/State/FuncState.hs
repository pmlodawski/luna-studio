---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.State.FuncState(
    FuncState(..),
    make
) where

import           Luna.Network.Graph.Graph          (Graph)
import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef)
import           Luna.Codegen.Hs.State.Context   as Context
import           Luna.Codegen.Hs.State.Context     (Context)
import           Luna.Codegen.Hs.State.Mode      as Mode
import           Luna.Codegen.Hs.State.Mode        (Mode)

data FuncState = FuncState {def      :: NodeDef,
							graph    :: Graph, 
                            mode     :: Mode, 
                            ctx      :: Context, 
                            lastctx  :: Context
                           } deriving (Show)

make :: NodeDef -> Graph -> FuncState
make d g = FuncState d g Mode.Auto Context.Pure Context.Pure
