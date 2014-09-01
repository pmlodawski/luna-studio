module Luna.Typechecker.Internal.TypeInference (Infer, split) where

import           Luna.Typechecker.Internal.AST.Type         (Tyvar(..))

import           Luna.Typechecker.Internal.Ambiguity        (defaultedPreds)
import           Luna.Typechecker.Internal.Assumptions      (Assump)
import           Luna.Typechecker.Internal.ContextReduction (reduce)
import           Luna.Typechecker.Internal.Substitutions    (Types(..))
import           Luna.Typechecker.Internal.TIMonad          (TI)
import           Luna.Typechecker.Internal.Typeclasses      (Pred,ClassEnv  )


import           Data.List                                          ((\\),partition)


type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)


split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])
split ce fs gs ps =  do ps'         <- reduce ce ps
                        let (ds, rs) = partition (all (`elem` fs) . tv) ps'
                        rs'         <- defaultedPreds ce (fs ++ gs) rs
                        return (ds, rs \\ rs')
