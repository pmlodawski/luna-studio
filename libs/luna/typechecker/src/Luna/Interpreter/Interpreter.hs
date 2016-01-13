module Luna.Interpreter.Interpreter where

import           Development.Placeholders
import qualified Luna.Interpreter.Dirty   as Dirty
import           Luna.Interpreter.Env     (Env)
import qualified Luna.Interpreter.Env     as Env
import           Luna.Interpreter.Monad   (InterpreterMonad, get)
import           Prologue


execute :: InterpreterMonad Env m => m ()
execute = do
    mapM_ Dirty.follow =<< Env.getReqNodes
    mapM_ run =<< topsort =<< Env.getReqNodes
    Env.clearReqNodes

connect prev next = $notImplemented

markModified = Dirty.markSuccessors

run = $notImplemented

topsort :: [a] -> m [a]
topsort = $notImplemented
