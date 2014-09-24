module Luna.Typechecker.Ambiguity (
    defaultedPreds, defaultSubst, candidates  
  ) where


import Luna.Typechecker.Substitutions    (Subst,Types(..))
import Luna.Typechecker.Typeclasses      (ClassEnv(..), Pred(..),entail)

import Luna.Typechecker.AST.TID          (TID)
import Luna.Typechecker.AST.Type         (Type(..), Tyvar)

import Luna.Typechecker.Internal.Logger

import Control.Monad                     (filterM)

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
  if    (all (TVar v ==) ts)         &&
        (any (`elem` numClasses) is) &&
        (all (`elem` stdClasses) is)
    then filterM (\t' -> mapM (entail ce []) [IsIn i t' | i <- is] >>= return . and ) (defaults ce)
    else return []
  --guard $ all (TVar v ==) ts
  --guard $ any (`elem` numClasses) is
  --guard $ all (`elem` stdClasses) is
  --let ts' = defaults ce
  --fltr <- mapM (\t' -> return (all (entail ce []) [IsIn i t' | i <- is])) ts'

  --if null fltr then mzero
  --return fltr
--candidates ce (v, qs) = [t' | let is = [i | IsIn i _ <- qs]
--                                  ts = [t | IsIn _ t <- qs],
--                              all (TVar v ==) ts,
--                              any (`elem` numClasses) is,
--                              all (`elem` stdClasses) is,
--                              t' <- defaults ce,
--                              all (entail ce []) [IsIn i t' | i <- is]]

withDefaults :: (Monad m) => ([Ambiguity] -> [Type] -> TCLoggerT m a) -> ClassEnv -> [Tyvar] -> [Pred] -> TCLoggerT m a
withDefaults f ce vs ps = do
  let vps = ambiguities ce vs ps
  tss <- mapM (candidates ce) vps
  if (any null tss)
    then throwError "cannot resolve ambiguity"
    else f vps (map head tss)

--withDefaults f ce vs ps | any null tss = fail "cannot resolve ambiguity"
--                        | otherwise = return (f vps (map head tss))
--  where vps = ambiguities ce vs ps
--        tss = map (candidates ce) vps

defaultedPreds :: (Monad m) => ClassEnv -> [Tyvar] -> [Pred] -> TCLoggerT m [Pred]
defaultedPreds = withDefaults (\vps _ -> return $ concatMap snd vps)

defaultSubst :: (Monad m) => ClassEnv -> [Tyvar] -> [Pred] -> TCLoggerT m Subst
defaultSubst = withDefaults (\vps ts -> return $ zip (map fst vps) ts)
