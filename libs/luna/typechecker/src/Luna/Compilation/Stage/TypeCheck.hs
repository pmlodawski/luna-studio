{-# LANGUAGE UndecidableInstances #-}

module Luna.Compilation.Stage.TypeCheck where

import Prelude.Luna

import qualified Luna.Compilation.Stage.Class as Stage
import           Luna.Compilation.Stage.Class hiding (runT, run)
import qualified Luna.Syntax.Name.Ident.Pool  as IdentPool
import           Luna.Syntax.Name.Ident.Pool  (IdentPoolT)


-- === Definitions === --

data TypeCheck = TypeCheck deriving (Show)

-- === Evaluation === --

type instance StageMonadT TypeCheck m = IdentPoolT m

instance Monad m => MonadStageT TypeCheck m where
    runT _ = flip IdentPool.evalT def


-- === Utils === --

runT :: MonadStageT TypeCheck m => StageMonadT TypeCheck m a -> m a
runT = Stage.runT TypeCheck

run :: MonadStage TypeCheck => StageMonad TypeCheck a -> a
run = Stage.run TypeCheck
