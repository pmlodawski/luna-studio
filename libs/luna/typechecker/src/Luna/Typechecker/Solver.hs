-- *------------------------------------------------
-- * Specification of the generic HM(X)
-- * type inference system in Haskell
-- *
-- * This instance deals with Ohori style records
-- *------------------------------------------------


module Luna.Typechecker.Solver (
    cs
  ) where


import Data.Default

import Luna.Typechecker.Data
import Luna.Typechecker.TypesAndConstraints
import Luna.Typechecker.Inference.Class
import Luna.Typechecker.StageTypecheckerState (report_error)


cs :: (Monad m) => (Subst, Constraint) -> StageTypecheckerPass m (Subst, Constraint)
cs (s, C c) =
 do (r,e) <- extract_predicates c
    (s',r') <- closure (s,r,e)
    b <- check_consistency r'
    if b then do c' <- apply s' (C c)
                 c'' <- simplify c'
                 return (s', c'')
     else report_error "inconsistent constraint" (def, C [TRUE])
cs _ = error "this case was not taken into account in original HM(Rec)"

-- divide predicates into record predicates and equality predicates

extract_predicates :: (Monad m) => [Predicate] ->  StageTypecheckerPass m ([Predicate],[Predicate])
extract_predicates [] = return ([],[])
extract_predicates (TRUE:p) = extract_predicates p
extract_predicates ((t `Subsume` t'):p) =
       do (r,e) <- extract_predicates p
          return (r, (t `Subsume` t'):e)


closure :: (Monad m) => (Subst,[Predicate],[Predicate]) ->  StageTypecheckerPass m (Subst,[Predicate])
closure (s, r, e) =
   do s' <- do_unify(s,e)
      c <- apply s' (C r)
      case c of
        C p1 -> do  e1 <- extract1 p1
                    e2 <- extract2 p1
                    p2 <- simplify_predicate (e1 ++ e2)
                    if null p2 then return (s',p1)
                     else closure (s', p1, p2)
        _    -> report_error "closure:uncompatible constraint" (def, [])


-- create subsumptions based on a label type of a particular record

extract1 :: (Monad m) => [Predicate] ->  StageTypecheckerPass m [Predicate]
extract1 [] = return []
extract1 (_:p) = extract1 p
get_extract1 :: (Monad m, Eq a) => [(a, Type)] -> a -> Type ->  StageTypecheckerPass m Predicate
get_extract1 [] _ _          = report_error "extract1:field label not found -> inconsistent constraint" (TV (TVar 0) `Subsume` TV (TVar 0))
get_extract1 ((l,t):f) l' t' = if l == l' then return (t `Subsume` t')
                               else get_extract1 f l' t'

-- Create subsumptions for each label based on all constraints for a particular label and record. All equations for a particular label in a record must be satisfied

extract2 :: (Monad m) => [Predicate] ->  StageTypecheckerPass m [Predicate]
extract2 [] = return []
extract2 (_:p) = extract2 p


check_consistency :: (Monad m) => [Predicate] ->  StageTypecheckerPass m Bool
check_consistency [] = return True
check_consistency (_: p) = check_consistency p



-- simplification of constraints

simplify :: (Monad m) => Constraint ->  StageTypecheckerPass m Constraint
simplify (C p) = do p' <- simplify_predicate p
                    return (C p')
simplify _ = error "this case was not taken into account in original HM(Rec)"

-- simplification of predicates

simplify_predicate :: (Monad m) => [Predicate] ->  StageTypecheckerPass m [Predicate]
simplify_predicate [] = return []
simplify_predicate ((t `Subsume` t'):p) =
  if t == t'  then simplify_predicate p
              else do p' <- simplify_predicate p
                      return ((t `Subsume` t'):p')
simplify_predicate (x:p) =
       do p' <- simplify_predicate p
          return (if x `elem` p' then p' else x : p')


do_unify :: (Monad m) => (Subst, [Predicate]) ->  StageTypecheckerPass m Subst
do_unify (s, []) = return s
do_unify (s, t `Subsume` t' : p) = do
    t1 <- apply s t
    t1' <- apply s t'
    s' <- unify (t1, t1')
    do_unify (s' `composeSubst` s, p)
do_unify (s,  _ : p ) = report_error "do_unify: predicate list not in normal form" def


unify :: (Monad m) => (Type, Type) ->  StageTypecheckerPass m Subst
unify (t1 `Fun` t2, t1' `Fun` t2') = do
    s1 <- unify (t1, t1')
    t3 <- apply s1 t2
    t3' <- apply s1 t2'
    s2 <- unify (t3, t3')
    return $ s2 `composeSubst` s1

unify (TV x, t) = varBind x t

unify (t, TV x) = unify (TV x, t)

-- unify (s, Record f, Record f') = g (s,f,f') where
--   g (s, [], []) = return s
--   g (s, (l,t):f, (l',t'):f') =
--     if l == l' then
--                do s' <- unify(s,t,t')
--                   g(s',f,f')
--     else report_error "not matching record" null_subst


varBind :: (Monad m) => TVar -> Type -> StageTypecheckerPass m Subst
varBind var typeV | typeV == TV var = return identitySubst
                  | var `elem` tv typeV = report_error "occurs check fails" identitySubst
                  | otherwise = return $ singleSubst var typeV
