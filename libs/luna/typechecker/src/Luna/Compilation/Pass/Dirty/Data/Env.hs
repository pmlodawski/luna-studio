module Luna.Compilation.Pass.Dirty.Data.Env where

import           Control.Monad.Trans.State
import           Prologue



data Env node = Env
        { _reqNodes :: [node]
        } deriving (Show)

makeLenses ''Env


addReqNode :: Monad m => node -> StateT (Env node) m ()
addReqNode node = modify (over reqNodes (node :))

clearReqNodes :: Monad m => StateT (Env node) m ()
clearReqNodes = modify (set reqNodes def)

getReqNodes :: Monad m => StateT (Env node) m [node]
getReqNodes = use reqNodes


instance Default (Env node) where
    def = Env def
