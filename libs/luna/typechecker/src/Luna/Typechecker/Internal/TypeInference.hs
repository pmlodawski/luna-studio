module Luna.Typechecker.Internal.TypeInference (Infer, split) where

import qualified Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Luna.Typechecker.Internal.Ambiguity        as Amb
import qualified Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Luna.Typechecker.Internal.ContextReduction as CxR
import qualified Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl


import           Data.List                                          ((\\),partition)


type Infer e t = Tcl.ClassEnv -> [Ass.Assump] -> e -> TIM.TI ([Tcl.Pred], t)


split :: Monad m => Tcl.ClassEnv -> [Ty.Tyvar] -> [Ty.Tyvar] -> [Tcl.Pred] -> m ([Tcl.Pred], [Tcl.Pred])
split ce fs gs ps =  do ps'         <- CxR.reduce ce ps
                        let (ds, rs) = partition (all (`elem` fs) . Sub.tv) ps'
                        rs'         <- Amb.defaultedPreds ce (fs ++ gs) rs
                        return (ds, rs \\ rs')
