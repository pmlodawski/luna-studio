module Flowbox.Luna.Typechecker.Internal.Ambiguity (defaultedPreds, defaultSubst) where

import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Flowbox.Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl

import           Flowbox.Luna.Typechecker.Internal.AST.TID          (TID)

import           Data.List                                          ((\\))


type Ambiguity = (Ty.Tyvar, [Tcl.Pred])

ambiguities :: Tcl.ClassEnv -> [Ty.Tyvar] -> [Tcl.Pred] -> [Ambiguity]
ambiguities _ vs ps = [(v, filter (elem v . Sub.tv) ps) | v <- Sub.tv ps \\ vs]





-- TODO [kgdk] 21 sie 2014: 
numClasses :: [TID]
numClasses = ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

-- TODO [kgdk] 21 sie 2014: 
stdClasses :: [TID]
stdClasses = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor", "Monad", "MonadPlus"] ++ numClasses

-- TODO [kgdk] 21 sie 2014: 
candidates :: Tcl.ClassEnv -> Ambiguity -> [Ty.Type]
candidates ce (v, qs) = [t' | let is = [i | Tcl.IsIn i _ <- qs]
                                  ts = [t | Tcl.IsIn _ t <- qs],
                              all (Ty.TVar v ==) ts,
                              any (`elem` numClasses) is,
                              all (`elem` stdClasses) is,
                              t' <- Tcl.defaults ce,
                              all (Tcl.entail ce []) [Tcl.IsIn i t' | i <- is]]

-- TODO [kgdk] 21 sie 2014: 
withDefaults :: Monad m => ([Ambiguity] -> [Ty.Type] -> a) -> Tcl.ClassEnv -> [Ty.Tyvar] -> [Tcl.Pred] -> m a
withDefaults f ce vs ps | any null tss = fail "cannot resolve ambiguity"
                        | otherwise = return (f vps (map head tss))
  where vps = ambiguities ce vs ps
        tss = map (candidates ce) vps

-- TODO [kgdk] 21 sie 2014: 
defaultedPreds :: Monad m => Tcl.ClassEnv -> [Ty.Tyvar] -> [Tcl.Pred] -> m [Tcl.Pred]
defaultedPreds = withDefaults (\vps _ -> concatMap snd vps)

-- TODO [kgdk] 21 sie 2014: 
defaultSubst :: Monad m => Tcl.ClassEnv -> [Ty.Tyvar] -> [Tcl.Pred] -> m Sub.Subst
defaultSubst = withDefaults (\vps ts -> zip (map fst vps) ts)
