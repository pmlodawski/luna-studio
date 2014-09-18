module Luna.Typechecker.AST.Pat (
    tiPat, Pat(..), tiPats
  ) where

import Luna.Typechecker.AST.Internal.Pat (Pat(..))

import Luna.Typechecker.AST.Kind         (Kind(..))
import Luna.Typechecker.AST.Lit          (tiLit)
import Luna.Typechecker.AST.Scheme       (toScheme)
import Luna.Typechecker.AST.Type         (Type, fn)

import Luna.Typechecker.Assumptions      (Assump(..))
import Luna.Typechecker.TIMonad          (TI, freshInst, unify, newTVar)
import Luna.Typechecker.Typeclasses      (Pred(..), Qual(..))


tiPat :: Pat -> TI ([Pred], [Assump], Type)
tiPat (PVar i)    = do v <- newTVar Star
                       return ([], [i :>: toScheme v], v)
tiPat PWildcard   = do v <- newTVar Star
                       return ([], [], v)
tiPat (PAs i pat) = do (ps, as, t) <- tiPat pat
                       return (ps, (i :>: toScheme t) : as, t)
tiPat (PLit l)    = do (ps, t) <- tiLit l
                       return (ps, [], t)
tiPat (PCon (_ :>: sc) pats)
                  = do (ps, as, ts) <- tiPats pats
                       t' <- newTVar Star
                       (qs :=> t) <- freshInst sc
                       unify t (foldr fn t' ts)
                       return (ps ++ qs, as, t')

tiPats :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats = do psasts <- mapM tiPat pats
                 let ps = concat [ps' | (ps', _,  _) <- psasts]
                     as = concat [as' | ( _, as', _) <- psasts]
                     ts =        [t   | ( _,  _,  t) <- psasts]
                 return (ps, as, ts)