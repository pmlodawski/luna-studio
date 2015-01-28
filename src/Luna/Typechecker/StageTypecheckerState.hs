module Luna.Typechecker.StageTypecheckerState (
    module StageTypecheckerStateClass,
    report_error
  ) where


import Control.Monad.State.Class                    (MonadState(..))

import Luna.Typechecker.StageTypecheckerState.Class as StageTypecheckerStateClass
import Luna.Typechecker.Inference.Class             (StageTypecheckerPass)



report_error :: (Monad m) => String -> a -> StageTypecheckerPass m a
report_error msg x = do
    st <- get
    let msgRes = "LUNA TC ERROR: " ++ msg ++ "\nState:\n\n" ++ show st
    fail msgRes