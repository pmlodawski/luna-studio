module Flowbox.Luna.Typechecker.Internal.Ambiguity (defaultedPreds, defaultSubst) where

--import qualified Flowbox.Luna.Typechecker.Internal.AST.Alternatives as Alt
--import qualified Flowbox.Luna.Typechecker.Internal.AST.AST          as AST
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Common       as Com
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr         as Exp
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind         as Knd
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Lit          as Lit
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Module       as Mod
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat          as Pat
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Scheme       as Sch
--import qualified Flowbox.Luna.Typechecker.Internal.AST.TID          as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

--import qualified Flowbox.Luna.Typechecker.Internal.Ambiguity        as Amb
--import qualified Flowbox.Luna.Typechecker.Internal.Assumptions      as Ass
--import qualified Flowbox.Luna.Typechecker.Internal.BindingGroups    as Bnd
--import qualified Flowbox.Luna.Typechecker.Internal.ContextReduction as CxR
--import qualified Flowbox.Luna.Typechecker.Internal.HasKind          as HKd
import qualified Flowbox.Luna.Typechecker.Internal.Substitutions    as Sub
--import qualified Flowbox.Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl
--import qualified Flowbox.Luna.Typechecker.Internal.TypeInference    as Inf
--import qualified Flowbox.Luna.Typechecker.Internal.Unification      as Unf

--import           Flowbox.Luna.Data.AST.Common                       (ID)
import           Flowbox.Luna.Typechecker.Internal.AST.TID          (TID)

import           Data.List                                          ((\\))


type Ambiguity = (Ty.Tyvar, [Tcl.Pred])

ambiguities :: Tcl.ClassEnv -> [Ty.Tyvar] -> [Tcl.Pred] -> [Ambiguity]
ambiguities ce vs ps = [(v, filter (elem v . Sub.tv) ps) | v <- Sub.tv ps \\ vs]





-- TODO [kgdk] 21 sie 2014: 
numClasses :: [TID]
numClasses = ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

-- TODO [kgdk] 21 sie 2014: 
stdClasses :: [TID]
stdClasses = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor", "Monad", "MonadPlus"] ++ numClasses

-- TODO [kgdk] 21 sie 2014: 
candidates :: Tcl.ClassEnv -> Ambiguity -> [Ty.Type]
candidates ce (v, qs) = [t' | let is = [i | Tcl.IsIn i t <- qs]
                                  ts = [t | Tcl.IsIn i t <- qs],
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
defaultedPreds = withDefaults (\vps ts -> concatMap snd vps)

-- TODO [kgdk] 21 sie 2014: 
defaultSubst :: Monad m => Tcl.ClassEnv -> [Ty.Tyvar] -> [Tcl.Pred] -> m Sub.Subst
defaultSubst = withDefaults (\vps ts -> zip (map fst vps) ts)
