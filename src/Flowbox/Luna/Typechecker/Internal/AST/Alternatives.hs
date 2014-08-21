module Flowbox.Luna.Typechecker.Internal.AST.Alternatives (Alt, tiAlts) where

import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr         as Exp
import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat          as Pat
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Flowbox.Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Flowbox.Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl
import qualified Flowbox.Luna.Typechecker.Internal.TypeInference    as Inf



type Alt = ([Pat.Pat], Exp.Expr)


tiAlt :: Inf.Infer Alt Ty.Type
tiAlt ce as (pats, e) = do (ps, as', ts) <- Pat.tiPats pats
                           (qs, t)       <- Exp.tiExpr ce (as' ++ as) e
                           return (ps ++ qs, foldr Ty.fn t ts)


-- TODO [kgdk] 20 sie 2014: rozszerzyć 'Infer' by dało się tutaj użyć
tiAlts :: Tcl.ClassEnv -> [Ass.Assump] -> [Alt] -> Ty.Type -> TIM.TI [Tcl.Pred]
tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
                         mapM_ (TIM.unify t . snd) psts
                         return (concatMap fst psts)

