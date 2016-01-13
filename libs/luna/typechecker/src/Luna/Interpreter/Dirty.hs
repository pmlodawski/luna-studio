module Luna.Interpreter.Dirty where

import           Development.Placeholders
import           Luna.Interpreter.Env     (Env)
import qualified Luna.Interpreter.Env     as Env
import           Luna.Interpreter.Monad   (InterpreterMonad, get)
import           Luna.Syntax.Repr.Graph   (Node, Ref)
import           Prologue


follow :: InterpreterMonad Env m => Ref Node -> m ()
follow node = do
    Env.addReqNode node
    $notImplemented

markSuccessors = do
    $notImplemented
