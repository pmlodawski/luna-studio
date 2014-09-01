module Luna.Typechecker.Internal.Ambiguity (defaultedPreds, defaultSubst) where

import           Luna.Typechecker.Internal.AST.Type         (Type(..), Tyvar)

import           Luna.Typechecker.Internal.Substitutions    (Subst,Types(..))
import           Luna.Typechecker.Internal.Typeclasses      (ClassEnv(..), Pred(..),entail)

import           Luna.Typechecker.Internal.AST.TID          (TID)

import           Data.List                                  ((\\))


type Ambiguity = (Tyvar, [Pred])

ambiguities :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities _ vs ps = [(v, filter (elem v . tv) ps) | v <- tv ps \\ vs]


numClasses :: [TID]
numClasses = ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

stdClasses :: [TID]
stdClasses = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor", "Monad", "MonadPlus"] ++ numClasses

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) = [t' | let is = [i | IsIn i _ <- qs]
                                  ts = [t | IsIn _ t <- qs],
                              all (TVar v ==) ts,
                              any (`elem` numClasses) is,
                              all (`elem` stdClasses) is,
                              t' <- defaults ce,
                              all (entail ce []) [IsIn i t' | i <- is]]

-- TODO [kgdk] 21 sie 2014: 
withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a) -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps | any null tss = fail "cannot resolve ambiguity"
                        | otherwise = return (f vps (map head tss))
  where vps = ambiguities ce vs ps
        tss = map (candidates ce) vps

-- TODO [kgdk] 21 sie 2014: 
defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds = withDefaults (\vps _ -> concatMap snd vps)

-- TODO [kgdk] 21 sie 2014: 
defaultSubst :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst = withDefaults (\vps ts -> zip (map fst vps) ts)
