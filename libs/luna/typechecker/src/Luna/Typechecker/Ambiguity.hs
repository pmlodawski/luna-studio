module Luna.Typechecker.Ambiguity (
    defaultedPreds, defaultSubst, candidates  
  ) where


import Luna.Typechecker.Substitutions    (Subst,Types(..))
import Luna.Typechecker.Typeclasses      (ClassEnv(..), Pred(..),entail)

import Luna.Typechecker.AST.TID          (TID)
import Luna.Typechecker.AST.Type         (Type(..), Tyvar)

import Luna.Typechecker.Internal.Logger

import Control.Monad                     (filterM,liftM)

import Data.List                         ((\\))


type Ambiguity = (Tyvar, [Pred])


ambiguities :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities _ vs ps = [(v, filter (elem v . tv) ps) | v <- tv ps \\ vs]


numClasses :: [TID]
numClasses = ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

stdClasses :: [TID]
stdClasses = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor", "Monad", "MonadPlus"] ++ numClasses

candidates :: (Monad m) => ClassEnv -> Ambiguity -> TCLoggerT m [Type]
candidates ce (v, qs) = do
  let is = [i | IsIn i _ <- qs]
      ts = [t | IsIn _ t <- qs]
  if    all (TVar v ==) ts         &&
        any (`elem` numClasses) is &&
        all (`elem` stdClasses) is
    then filterM (\t' -> liftM and $ mapM (entail ce []) [IsIn i t' | i <- is]) (defaults ce)
    else return []

withDefaults :: (Monad m) => ([Ambiguity] -> [Type] -> TCLoggerT m a) -> ClassEnv -> [Tyvar] -> [Pred] -> TCLoggerT m a
withDefaults f ce vs ps = do
  let vps = ambiguities ce vs ps
  tss <- mapM (candidates ce) vps
  if any null tss
    then throwError "cannot resolve ambiguity"
    else f vps (map head tss)

defaultedPreds :: (Monad m) => ClassEnv -> [Tyvar] -> [Pred] -> TCLoggerT m [Pred]
defaultedPreds = withDefaults (\vps _ -> return $ concatMap snd vps)

defaultSubst :: (Monad m) => ClassEnv -> [Tyvar] -> [Pred] -> TCLoggerT m Subst
defaultSubst = withDefaults (\vps ts -> return $ zip (map fst vps) ts)
