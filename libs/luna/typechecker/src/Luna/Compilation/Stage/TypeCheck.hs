{-# LANGUAGE UndecidableInstances #-}

module Luna.Compilation.Stage.TypeCheck where

import Prelude.Luna

import qualified Luna.Compilation.Stage.Class as Stage
import           Luna.Compilation.Stage.Class hiding (runT, run)
import qualified Luna.Syntax.Name.Ident.Pool  as IdentPool
import           Luna.Syntax.Name.Ident.Pool  (IdentPoolT)

import           Luna.Compilation.Stage.TypeCheck.Class (TypeCheckT)
import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck


-- === Definitions === --

data TypeCheck t = TypeCheck deriving (Show)

class Monad m => TypeCheckerPass p m where
    hasJobs :: p -> m Bool

    runTCPass :: p -> m ProgressStatus
    runTCPass p = runTCWithArtifacts p $ return ()

    runTCWithArtifacts :: p -> m () -> m ProgressStatus
    runTCWithArtifacts p art = (runTCPass p) <* art

data ProgressStatus = Progressed | Stuck deriving (Show, Eq)

-- === Evaluation === --

type instance StageMonadT (TypeCheck t) m = TypeCheckT t $ IdentPoolT m

instance Monad m => MonadStageT (TypeCheck t) m where
    runT _ = flip IdentPool.evalT def . flip TypeCheck.evalT def

-- === Utils === --

runT :: MonadStageT (TypeCheck t) m => StageMonadT (TypeCheck t) m a -> m a
runT = Stage.runT TypeCheck

run :: MonadStage (TypeCheck t) => StageMonad (TypeCheck t) a -> a
run = Stage.run TypeCheck

-- === Pass Combinators === --

data Loop a = Loop a deriving (Show, Eq)

instance TypeCheckerPass a m => TypeCheckerPass (Loop a) m where
    hasJobs (Loop a) = hasJobs a

    runTCWithArtifacts (Loop a) art = do
        shouldStart <- hasJobs a
        res <- if shouldStart then runTCWithArtifacts a art else return Stuck
        case res of
            Stuck -> return Stuck
            _     -> runTCWithArtifacts (Loop a) art
