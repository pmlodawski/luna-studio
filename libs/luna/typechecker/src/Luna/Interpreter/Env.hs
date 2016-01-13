module Luna.Interpreter.Env where

import Luna.Interpreter.Monad (InterpreterMonad, get, withEnvM_)
import Luna.Syntax.Repr.Graph (Node, Ref)
import Prologue



data Env = Env
         { _reqNodes :: [Ref Node]
         } deriving (Show)

makeLenses ''Env

addReqNode :: InterpreterMonad Env m => Ref Node -> m ()
addReqNode node = withEnvM_ (return . over reqNodes (node :))

clearReqNodes :: InterpreterMonad Env m => m ()
clearReqNodes = withEnvM_ (return . set reqNodes def)

getReqNodes :: InterpreterMonad Env m => m [Ref Node]
getReqNodes = view reqNodes <$> get
