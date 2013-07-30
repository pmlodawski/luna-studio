---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.State.DefState(
    FuncState(..),
    make
) where

import           Luna.Network.Graph.Graph          (Graph)
import           Luna.Codegen.State.Context      as Context
import           Luna.Codegen.State.Context        (Context)
import           Luna.Codegen.State.Mode         as Mode
import           Luna.Codegen.State.Mode           (Mode)

data DefState = DefState {manager  :: DefManager, 
                          path     :: Path, 
                         } deriving (Show)

make :: NodeDef -> DefManager -> DefState
make def = FuncState g Mode.Auto Context.Pure Context.Pure
