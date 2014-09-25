module Luna.Typechecker (
    tiProgram
  ) where

import Luna.Typechecker.Ambiguity        (defaultSubst)
import Luna.Typechecker.Assumptions      (Assump)
import Luna.Typechecker.BindingGroups    (BindGroup,tiSeq,tiBindGroup)
import Luna.Typechecker.ContextReduction (reduce)
import Luna.Typechecker.Substitutions    (Types(..),(@@))
import Luna.Typechecker.TIMonad          (startTI,getSubst)
import Luna.Typechecker.Typeclasses      (ClassEnv)

import Luna.Typechecker.Internal.Logger


type Program = [BindGroup]


tiProgram :: ClassEnv -> [Assump] -> Program -> (Either String [Assump], String)
tiProgram ce as bgs = let (res, stack) = startTI $ runLoggerT $ do (ps, as') <- tiSeq tiBindGroup ce as bgs
                                                                   s         <- getSubst
                                                                   rs        <- reduce ce (apply s ps)
                                                                   s'        <- defaultSubst ce [] rs
                                                                   return    $  apply (s' @@ s) as'
                       in (res, formatStack True stack)
