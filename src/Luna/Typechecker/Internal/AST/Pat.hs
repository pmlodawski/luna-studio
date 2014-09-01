module Luna.Typechecker.Internal.AST.Pat (tiPat, Pat(..), tiPats) where

import           Luna.Typechecker.Internal.AST.Kind         (Kind(..))
import           Luna.Typechecker.Internal.AST.Lit          (Lit,tiLit)
import           Luna.Typechecker.Internal.AST.Scheme       (toScheme)
import           Luna.Typechecker.Internal.AST.Type         (Type, fn)

import           Luna.Typechecker.Internal.Assumptions      (Assump(..))
import           Luna.Typechecker.Internal.TIMonad          (TI, freshInst, unify, newTVar)
import           Luna.Typechecker.Internal.Typeclasses      (Pred(..), Qual(..))

import           Luna.Typechecker.Internal.AST.TID          (TID)

data Pat = PVar TID
         | PWildcard
         | PAs TID Pat
         | PLit Lit
         | PCon Assump [Pat]
         deriving (Show)

tiPat :: Pat -> TI ([Pred], [Assump], Type)
tiPat (PVar i) = do v <- newTVar Star
                    return ([], [i :>: toScheme v], v)
tiPat PWildcard = do v <- newTVar Star
                     return ([], [], v)
tiPat (PAs i pat) = do (ps, as, t) <- tiPat pat
                       return (ps, (i :>: toScheme t) : as, t)
tiPat (PLit l) = do (ps, t) <- tiLit l
                    return (ps, [], t)
tiPat (PCon (_ :>: sc) pats) = do (ps, as, ts) <- tiPats pats
                                  t' <- newTVar Star
                                  (qs :=> t) <- freshInst sc
                                  unify t (foldr fn t' ts)
                                  return (ps ++ qs, as, t')


-- TODO [kgdk] 20 sie 2014: które lepsze/wydajniejsze?
--tiPats pats = do psasts <- mapM tiPat pats
--                 let ps = concatMap (^. _1) psasts
--                     as = concatMap (^. _2) psasts
--                     ts =       map (^. _3) psasts
--                 return (ps, as, ts)
--tiPats pats = do psasts <- mapM tiPat pats
--                 return $ (concatMap (^. _1) psasts, concatMap (^. _2) psasts, psasts^..traverse._3)
--tiPats pats = do psasts <- mapM tiPat pats
--                 return (psasts^..traverse._1 & concat, psasts^..traverse._2 & concat, psasts^..traverse._3)
--tiPats pats = do psasts <- mapM tiPat pats
--                 return $ _1%~concat $ _2%~concat $ foldr (\(a,b,c) (x,y,z) -> (a:x, b:y, c:z)) ([],[],[]) psasts
tiPats :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats = do psasts <- mapM tiPat pats
                 let ps = concat [ps' | (ps', _,  _) <- psasts]
                     as = concat [as' | ( _, as', _) <- psasts]
                     ts =        [t   | ( _,  _,  t) <- psasts]
                 return (ps, as, ts)


