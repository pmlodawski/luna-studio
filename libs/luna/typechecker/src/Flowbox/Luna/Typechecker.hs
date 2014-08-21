module Flowbox.Luna.Typechecker where



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
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Flowbox.Luna.Typechecker.Internal.Ambiguity        as Amb
import qualified Flowbox.Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Flowbox.Luna.Typechecker.Internal.BindingGroups    as Bnd
import qualified Flowbox.Luna.Typechecker.Internal.ContextReduction as CxR
--import qualified Flowbox.Luna.Typechecker.Internal.HasKind          as HKd
import qualified Flowbox.Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Flowbox.Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl
--import qualified Flowbox.Luna.Typechecker.Internal.TypeInference    as Inf
--import qualified Flowbox.Luna.Typechecker.Internal.Unification      as Unf

--import           Flowbox.Luna.Data.AST.Common                       (ID)
--import           Flowbox.Luna.Typechecker.Internal.AST.TID          (TID)

-- TODO [kgdk] 15 sie 2014: czy let-polymorphism vs lambda-monomorphism nie jest popsute?


-- TODO [kgdk] 20 sie 2014: czy może Mod.Module == Program?
type Program = [Bnd.BindGroup]



tiProgram :: Tcl.ClassEnv -> [Ass.Assump] -> Program -> [Ass.Assump]
tiProgram ce as bgs = TIM.runTI $ do (ps, as') <- Bnd.tiSeq Bnd.tiBindGroup ce as bgs
                                     s <- TIM.getSubst
                                     rs <- CxR.reduce ce (Sub.apply s ps)
                                     s' <- Amb.defaultSubst ce [] rs
                                     return (Sub.apply (s' Sub.@@ s) as')
