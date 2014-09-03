module Luna.Typechecker.Internal.TypeInference (Infer, split) where

import Luna.Typechecker.Internal.AST.Type         (Tyvar(..))

import Luna.Typechecker.Internal.Ambiguity        (defaultedPreds)
import Luna.Typechecker.Internal.Assumptions      (Assump)
import Luna.Typechecker.Internal.ContextReduction (reduce)
import Luna.Typechecker.Internal.Substitutions    (Types(..))
import Luna.Typechecker.Internal.TIMonad          (TI)
import Luna.Typechecker.Internal.Typeclasses      (Pred,ClassEnv(..))


import Data.List                                  ((\\),partition,nubBy,intercalate)
import Data.Function                              (on)

import Debug.Trace
import Text.Printf                                (printf)

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)


split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred] -> m ([Pred], [Pred])
split ce fs gs ps =  do let splitter = "\n                "
                            cen = intercalate (splitter ++ "   ") $ map show $ nubBy ((==) `on` fst) $ classes_names ce
                        traceM $ printf "DBG: split%sce=%s%sfixed-variables=%s%sto-quantify-vars=%s%spredicates=%s" splitter cen splitter (show fs) splitter (show gs) splitter (show ps)
                        ps'         <- reduce ce ps  -- <<#########################################################
                        traceM $ printf "DBG:  >> ps' <- reduce ce ps   ==   %s" (show ps')
                        let (ds, rs) = partition (all (`elem` fs) . tv) ps'  -- <<#################################
                        traceM $ printf "DBG:  >> partition (all (`elem` fs) . tv) ps' = (ds, rs) == (%s, %s)" (show ds) (show rs) 
                        rs'         <- defaultedPreds ce (fs ++ gs) rs  -- <<######################################
                        traceM $ printf "DBG:  >> deferred: %s // retained: %s" (show ds) (show (rs \\ rs'))
                        return (ds, rs \\ rs')  -- <<##############################################################
