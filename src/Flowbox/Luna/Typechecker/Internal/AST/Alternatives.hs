module Flowbox.Luna.Typechecker.Internal.AST.Alternatives (Alt, tiAlts) where

--import qualified Flowbox.Luna.Typechecker.Internal.AST.Alternatives as Alt
--import qualified Flowbox.Luna.Typechecker.Internal.AST.AST          as AST
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Common       as Com
import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr         as Exp
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind         as Knd
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Lit          as Lit
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Module       as Mod
import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat          as Pat
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Scheme       as Sch
--import qualified Flowbox.Luna.Typechecker.Internal.AST.TID          as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

--import qualified Flowbox.Luna.Typechecker.Internal.Ambiguity        as Amb
import qualified Flowbox.Luna.Typechecker.Internal.Assumptions      as Ass
--import qualified Flowbox.Luna.Typechecker.Internal.BindingGroups    as Bnd
--import qualified Flowbox.Luna.Typechecker.Internal.ContextReduction as CxR
--import qualified Flowbox.Luna.Typechecker.Internal.HasKind          as HKd
--import qualified Flowbox.Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Flowbox.Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl
import qualified Flowbox.Luna.Typechecker.Internal.TypeInference    as Inf
--import qualified Flowbox.Luna.Typechecker.Internal.Unification      as Unf

--import           Flowbox.Luna.Data.AST.Common                       (ID)
--import           Flowbox.Luna.Typechecker.Internal.AST.TID          (TID(..))


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

