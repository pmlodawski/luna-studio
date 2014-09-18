module Luna.Typechecker (
    tiProgram
  ) where

import Luna.Typechecker.Ambiguity        (defaultSubst)
import Luna.Typechecker.Assumptions      (Assump)
import Luna.Typechecker.BindingGroups    (BindGroup,tiSeq,tiBindGroup)
import Luna.Typechecker.ContextReduction (reduce)
import Luna.Typechecker.Substitutions    (Types(..),(@@))
import Luna.Typechecker.TIMonad          (runTI,getSubst)
import Luna.Typechecker.Typeclasses      (ClassEnv)


type Program = [BindGroup]



tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
tiProgram ce as bgs = runTI $ do (ps, as') <- tiSeq tiBindGroup ce as bgs
                                 s         <- getSubst
                                 rs        <- reduce ce (apply s ps)
                                 s'        <- defaultSubst ce [] rs
                                 return    $  apply (s' @@ s) as'
