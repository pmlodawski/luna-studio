module Luna.Typechecker where

import Luna.Typechecker.Internal.Ambiguity        (defaultSubst)
import Luna.Typechecker.Internal.Assumptions      (Assump)
import Luna.Typechecker.Internal.BindingGroups    (BindGroup,tiSeq,tiBindGroup)
import Luna.Typechecker.Internal.ContextReduction (reduce)
import Luna.Typechecker.Internal.Substitutions    (Types(..),(@@))
import Luna.Typechecker.Internal.TIMonad          (runTI,getSubst)
import Luna.Typechecker.Internal.Typeclasses      (ClassEnv)


type Program = [BindGroup]



tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
tiProgram ce as bgs = runTI $ do (ps, as') <- tiSeq tiBindGroup ce as bgs
                                 s         <- getSubst
                                 rs        <- reduce ce (apply s ps)
                                 s'        <- defaultSubst ce [] rs
                                 return    $  apply (s' @@ s) as'
