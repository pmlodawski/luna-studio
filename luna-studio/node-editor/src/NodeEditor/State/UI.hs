{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.State.UI where

import           Common.Prelude
import           NodeEditor.React.Model.App     (App)
import           NodeEditor.React.Store         (Ref)


data State = State { _app                  :: Ref App
                   , _oldApp               :: App
                   , _renderNeeded         :: Bool
                   }

makeLenses ''State

mkState :: Ref App -> State
mkState ref = State
    {- app          -} ref
    {- oldApp       -} def
    {- renderNeeded -} False
