module Luna.Typechecker where




import qualified Luna.Typechecker.Internal.Ambiguity        as Amb
import qualified Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Luna.Typechecker.Internal.BindingGroups    as Bnd
import qualified Luna.Typechecker.Internal.ContextReduction as CxR
import qualified Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl


-- TODO [kgdk] 15 sie 2014: czy let-polymorphism vs lambda-monomorphism nie jest popsute?


-- TODO [kgdk] 20 sie 2014: czy może Mod.Module == Program?
type Program = [Bnd.BindGroup]



tiProgram :: Tcl.ClassEnv -> [Ass.Assump] -> Program -> [Ass.Assump]
tiProgram ce as bgs = TIM.runTI $ do (ps, as') <- Bnd.tiSeq Bnd.tiBindGroup ce as bgs
                                     s <- TIM.getSubst
                                     rs <- CxR.reduce ce (Sub.apply s ps)
                                     s' <- Amb.defaultSubst ce [] rs
                                     return (Sub.apply (s' Sub.@@ s) as')
