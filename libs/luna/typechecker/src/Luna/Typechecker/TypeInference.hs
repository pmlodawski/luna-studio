module Luna.Typechecker.TypeInference (
    Infer, split
  ) where

import Luna.Typechecker.AST.Type         (Tyvar(..))

import Luna.Typechecker.Ambiguity        (defaultedPreds)
import Luna.Typechecker.Assumptions      (Assump)
import Luna.Typechecker.ContextReduction (reduce)
import Luna.Typechecker.Substitutions    (Types(..))
import Luna.Typechecker.TIMonad          (TI)
import Luna.Typechecker.Typeclasses      (Pred,ClassEnv(..))


import Data.List                                  ((\\),partition)

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)


split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])
split ce fs gs ps =  do ps'         <- reduce ce ps
                        let (ds, rs) = partition (all (`elem` fs) . tv) ps'
                        rs'         <- defaultedPreds ce (fs ++ gs) rs
                        return (ds, rs \\ rs')
