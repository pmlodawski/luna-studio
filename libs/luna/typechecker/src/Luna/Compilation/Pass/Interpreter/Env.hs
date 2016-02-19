module Luna.Compilation.Pass.Interpreter.Env where

import           Prologue

import           Luna.Compilation.Pass.Interpreter.Class


data Env node = Env { _reqNodes :: [node]
                    } deriving Show

makeLenses ''Env


addReqNode :: InterpreterMonad (Env node) m => node -> m ()
addReqNode node = modify_ (over reqNodes (node :))

clearReqNodes :: InterpreterMonad (Env node) m => m ()
clearReqNodes = modify_ (set reqNodes def)

getReqNodes :: InterpreterMonad (Env node) m => m [node]
getReqNodes = view reqNodes <$> get


instance Default (Env node) where
    def = Env def
