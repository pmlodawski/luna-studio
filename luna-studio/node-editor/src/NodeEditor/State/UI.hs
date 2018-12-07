module  NodeEditor.State.UI
    ( State
    , app
    , modify
    , renderIfNeeded
    ) where

import           Common.Prelude
import           NodeEditor.React.Model.App     (App)
import qualified Control.Monad.State as M


data State = State { _current      :: App
                   , _rendered     :: App
                   , _renderNeeded :: Bool
                   }

makeLenses ''State

instance Default State where
    def = State def def False

app :: Getter State App
app = current

modify :: M.State App r -> State -> (r, State)
modify action state = let
    (result, new) = M.runState action $ state ^. current
    newState = state & current .~ new
                     & renderNeeded .~ True
    in (result, newState)

renderIfNeeded :: Monad m => (App -> App -> m ()) -> State -> m State
renderIfNeeded renderer state =  if state ^. renderNeeded
    then do
        renderer (state ^. rendered) (state ^. current)
        return $ state & renderNeeded .~ False
                       & rendered .~ (state ^. current)
    else
        return state
