module Luna.Typechecker.TypeInference (
    Infer, split
  ) where


import Luna.Typechecker.Ambiguity        (defaultedPreds)
import Luna.Typechecker.Assumptions      (Assump)
import Luna.Typechecker.ContextReduction (reduce)
import Luna.Typechecker.Substitutions    (Types(..))
import Luna.Typechecker.TIMonad          (TI)
import Luna.Typechecker.Typeclasses      (Pred,ClassEnv(..))

import Luna.Typechecker.AST.Type         (Tyvar(..))

import Luna.Typechecker.Internal.Logger


import Control.Monad.Trans               (lift)

import Data.List                         ((\\),partition)


type Infer e t = ClassEnv -> [Assump] -> e -> TCLoggerT TI ([Pred], t)


split :: (Monad m) => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> TCLoggerT m ([Pred], [Pred])
split ce fs gs ps =  do ps'         <- reduce ce ps
                        let (ds, rs) = partition (all (`elem` fs) . tv) ps'
                        rs'         <- defaultedPreds ce (fs ++ gs) rs
                        return (ds, rs \\ rs')
