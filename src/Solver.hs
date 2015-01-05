-- *------------------------------------------------
-- * Specification of the generic HM(X)
-- * type inference system in Haskell
-- *
-- * This instance deals with Ohori style records
-- *------------------------------------------------


module Solver where


import Control.Applicative
import Control.Monad

import Data.Functor.Identity

-- *------------------------------------------------
-- * Category: DATA DECLARATIONS
-- *------------------------------------------------

-- The type language

type TVar       = Int
type Var        = Int
type Fieldlabel = Var
type Field      = (Fieldlabel, Type)
data Type       = TV TVar
                | Type `Fun` Type
                | Record [Field]
                    deriving (Show,Eq)


-- Note, record fields are sorted wrt field label


-- The constraint language

data Predicate  = TRUE
                | Type `Subsume` Type
                | Reckind Type Fieldlabel Type
                    deriving (Show,Eq)

data Constraint = C [Predicate]
                | Proj [TVar] [Predicate]
                     deriving Show


-- FILL IN: extend type and constraint language


-- Type schemes

data TypeScheme = Mono Type
                | Poly [TVar] Constraint Type
                     deriving Show

-- Substitutions and type environments

type Subst      = [(TVar, Type)]

type Typo       = [(Var,TypeScheme)]

-- The term language

--data Term       = Id Var | Abs Var Term | App Term Term
--                | Let Var Term Term
--                     deriving Show

-- The error monad

data E a        = Suc a | Err String
                     deriving Show

-- The type inference monad - a typing problem consists of a three tuple

data TP a = TP ( (TVar, Subst, Constraint) ->
                 E (TVar, Subst, Constraint, a))


-- *----------------------------------------
-- * Category: CLASS DECLARATIONS
-- *----------------------------------------


class TypesAndConstraints c where
 apply :: Subst -> c -> TP c
 tv    :: c -> [TVar]

-- apply -- applies a substitution to either a type or constraint
-- tv -- computes the free type variables

-- *-----------------------------------------
-- * Category: INSTANCE DECLARATIONS
-- *-----------------------------------------


instance TypesAndConstraints Predicate where
  apply s TRUE              = return TRUE
  apply s (t1 `Subsume` t2) = do t1' <- apply s t1
                                 t2' <- apply s t2
                                 return (t1' `Subsume` t2')
  apply s (Reckind t l t')  = do t1 <- apply s t
                                 t1' <- apply s t'
                                 return (Reckind t1 l t1')
  tv TRUE                   = []
  tv (t1 `Subsume` t2)      = tv t1 ++ tv t2
  tv (Reckind t l t')       = tv t ++ tv t'

instance TypesAndConstraints c => TypesAndConstraints [c] where
  apply s  = mapM (apply s)
  tv       = foldl f [] where
                       f z x = z ++ tv x

instance TypesAndConstraints Constraint where
     apply s (C p)            = do p' <- apply s p
                                   return (C p')
     apply s (Proj tvl p)      = do p' <- apply s p
                                    return (Proj tvl p')
     tv (C p)                 = tv p
     tv (Proj tvl p)           = without (tv p)  tvl


instance TypesAndConstraints Type where
  apply s (TV tvl)           = case lookup tvl s of
                                  Just t  -> return t
                                  Nothing -> return (TV tvl)
  apply s (t1 `Fun` t2)     = do t1' <- apply s t1
                                 t2' <- apply s t2
                                 return (t1' `Fun` t2')
  apply s (Record [])        = return (Record [])
  apply s (Record ((l,t):f)) = do f' <- apply s (Record f)
                                  case f' of
                                    Record f'' -> do t' <- apply s t
                                                     return (Record ((l,t'):f''))
                                    _          -> report_error "apply:uncompatible types" (Record [])
  tv (TV tvl)                = [tvl]
  tv (t1 `Fun` t2)          = tv t1 ++ tv t2
  tv (Record [])            = []
  tv (Record ((l,t):f))     = tv t ++ tv (Record f)


instance TypesAndConstraints TypeScheme where
  apply s (Poly tvl c t)     = do c' <- apply s c
                                  t' <- apply s t
                                  return (Poly tvl c' t')
  apply s (Mono t)          = do t' <- apply s t
                                 return (Mono t')
  tv (Poly tvl c t)          = without (tv t ++ tv c) tvl
  tv (Mono t)               = tv t


-- FILL IN: instance declarations in case of extended type and
--          constraint language


-- *--------------------------------------------------
-- * Category: MONAD DECLARATIONS
-- *--------------------------------------------------

unTP :: TP t -> (TVar, Subst, Constraint) -> E (TVar, Subst, Constraint, t)
unTP (TP a) = a


instance Monad TP where
 m >>= k = TP ( \ (n,s,c) ->
                     case unTP m (n,s,c) of
                       Suc (n',s',c',x) -> unTP (k x) (n',s',c')
                       Err se           -> Err se )
 return x = TP ( \ (n,s,c) -> return (n,s,c,x) )


instance Functor TP where
  fmap f m = TP $ \(n,s,c) ->
                     case unTP m (n,s,c) of
                       Suc (n',s',c',x) -> Suc (n',s',c',f x)
                       Err se           -> Err se

instance Applicative TP where
  pure = return
  (<*>) = ap


instance Monad E where
 m >>= k = case m of
           Suc a -> k a
           Err s -> Err s
 return  = Suc

instance Functor E where
  fmap f m = case m of
           Suc a -> Suc (f a)
           Err s -> Err s

instance Applicative E where
  pure = return
  (<*>) = ap


report_error :: String -> a -> TP a
report_error msg x =  TP ( \ (n,s,c) -> Err msg)


-- *---------------------------------------
-- * Category: INITIALIZATIONS
-- *---------------------------------------


null_subst :: Subst
null_subst = []

init_typo :: Typo

-- FILL IN: initial type environment

true_cons :: Constraint
true_cons = C [TRUE]

init_tvar :: TVar
init_tvar = 0

new_tvar :: TVar -> TVar
new_tvar n = n + 1



-- *--------------------------------------
-- * Category: Core type inferencer
-- *--------------------------------------


-- primitives for lists

without :: [TVar] -> [TVar] -> [TVar]
without [] a    = []
without (x:a) b = if x `elem` b then without a b
                  else x : without a b



-- primitives for dealing with constraints

add_cons :: Constraint -> Constraint -> Constraint
add_cons (C p1) (C p2)               = C (p1 ++ p2)
add_cons (C p1) (Proj tvr p2)        = Proj tvr (p1 ++ p2)
add_cons (Proj tvr p1) (C p2)        = Proj tvr (p1 ++ p2)
add_cons (Proj tv1 p1) (Proj tv2 p2) = Proj (tv1 ++ tv2) (p1 ++ p2)


projection :: Constraint -> [TVar] -> Constraint

-- FILL IN: definition of projection on constraints.
--          Projection might vary depending on different
--          kinds of constraint systems. A standard
--          definition can be found below.

--          projection (C p) tvl         = Proj tvl p
--          projection (Proj tv1 p) tv2 = Proj (tv1 ++ tv2) p


-- lifted functions


tv_typo :: Typo -> [TVar]
tv_typo = foldl f [] where
          f z (v,ts) = z ++ tv ts

add_constraint :: Constraint -> TP ()
add_constraint c1 =
     TP (\ (n,s,c) -> return (n,s,add_cons c c1,()))



-- handling of type variables and type environments

newtvar :: TP TVar
newtvar = TP (\ (n,s,c) -> return (new_tvar n,s,c,n) )

insert :: Typo -> (Var, TypeScheme) -> Typo
insert a (x,t) = (x,t):a

mylookup :: Typo -> Var -> E TypeScheme
mylookup [] y =
    Err "undeclared variable"
mylookup ((x,t):xs) y =
      if x == y then return t
      else mylookup xs y



-- instantiation and generalization



rename :: TP Subst -> TVar -> TP Subst
rename s x = do newtv <- newtvar
                s' <- s
                return ((x, TV newtv):s')

inst :: Typo -> Var -> TP Type
inst env x =
   case mylookup env x of
     Suc ts -> case ts of
                Mono t        -> return t
                Poly tvl c t  -> do s' <- foldl rename (return null_subst) tvl
                                    c' <- apply s' c
                                    t' <- apply s' t
                                    add_constraint c'
                                    return t'
     Err _  -> do ntv <- newtvar
                  report_error "undeclared variable" (TV ntv)






gen :: Typo -> Type -> TP TypeScheme
gen env t =
 TP ( \ (n,s,c) -> return (n,s, projection c (fv t c env), Poly (fv t c env) c t) )
                      where fv t1 c1 env1 = without (tv t1 ++ tv c1) (tv_typo env1)



-- constraint solver



cs :: (Subst, Constraint) -> TP (Subst, Constraint)


-- that's what you need to supply


-- incorporating the constraint solver into monad TP

normalize :: Type -> TP Type
normalize a = do s <- get_subst
                 c <- get_cons
                 (s',c') <- cs (s,c)
                 t <- apply s' a
                 return_result s' c' t



get_subst :: TP Subst
get_subst = TP ( \ (n,s,c) -> return (n,s,c,s))

get_cons :: TP Constraint
get_cons = TP ( \ (n,s,c) -> return (n,s,c,c))

return_result :: Subst -> Constraint -> Type -> TP Type
return_result s c t = TP ( \ (n,s',c') -> return (n,s,c,t))






init_typo = []

-- Invariant: all constraints are true, hence projection is always trivial

projection _ _ = true_cons



cs (s, C c) =
 do (r,e) <- extract_predicates c
    (s',r') <- closure (s,r,e)
    b <- check_consistency r'
    if b then do c' <- apply s' (C c)
                 c'' <- simplify c'
                 return (s', c'')
     else report_error "inconsistent constraint" (null_subst, C [TRUE])
cs _ = error "this case was not taken into account in original HM(Rec)"

-- divide predicates into record predicates and equality predicates

extract_predicates :: [Predicate] -> TP ([Predicate],[Predicate])
extract_predicates [] = return ([],[])
extract_predicates (TRUE:p) = extract_predicates p
extract_predicates ((t `Subsume` t'):p) =
       do (r,e) <- extract_predicates p
          return (r, (t `Subsume` t'):e)
extract_predicates (Reckind t l t' : p) =
       do (r,e) <- extract_predicates p
          return ( Reckind t l t' : r, e)



closure ::  (Subst,[Predicate],[Predicate]) -> TP (Subst,[Predicate])
closure (s, r, e) =
   do s' <- do_unify(s,e)
      c <- apply s' (C r)
      case c of
        C p1 -> do  e1 <- extract1 p1
                    e2 <- extract2 p1
                    p2 <- simplify_predicate (e1 ++ e2)
                    if null p2 then return (s',p1)
                     else closure (s', p1, p2)
        _    -> report_error "closure:uncompatible constraint" (null_subst, [])


-- create subsumptions based on a label type of a particular record

extract1 :: [Predicate] -> TP [Predicate]
extract1 [] = return []
extract1 (Reckind (Record f) l t : p) =
    do e <- get_extract1 f l t
       e' <- extract1 p
       return (e:e')
extract1 (_:p) = extract1 p
get_extract1 :: Eq a => [(a, Type)] -> a -> Type -> TP Predicate
get_extract1 [] _ _          = report_error "extract1:field label not found -> inconsistent constraint" (TV 0 `Subsume` TV 0)
get_extract1 ((l,t):f) l' t' = if l == l' then return (t `Subsume` t')
                               else get_extract1 f l' t'

-- Create subsumptions for each label based on all constraints for a particular label and record. All equations for a particular label in a record must be satisfied

extract2 :: [Predicate] -> TP [Predicate]
extract2 [] = return []
extract2 (Reckind t l t' : p) =
    do e <- extract2 p
       e' <- get_extract2 p t l t'
       return (e ++ e')
extract2 (_:p) = extract2 p
get_extract2 :: Monad m => [Predicate] -> Type -> Fieldlabel -> Type -> m [Predicate]
get_extract2 [] _ _ _ = return []
get_extract2 (Reckind a l a' : p) t l' t' = if (l == l') && (a == t)
                then do e <- get_extract2 p t l' t'
                        return ((a' `Subsume` t'):e)
                else get_extract2 p t l' t'
get_extract2 (_:p) t l t'                 = get_extract2 p t l t'



check_consistency :: [Predicate] -> TP Bool
check_consistency [] = return True
check_consistency (Reckind (Record f) l t' : p) = check_consistency p
check_consistency (Reckind (TV a) l t'     : p) = check_consistency p
check_consistency (Reckind _ l t'          : p) = return False
check_consistency (_                       : p) = check_consistency p



-- simplification of constraints

simplify :: Constraint -> TP Constraint
simplify (C p) = do p' <- simplify_predicate p
                    return (C p')
simplify _ = error "this case was not taken into account in original HM(Rec)"

-- simplification of predicates

simplify_predicate :: [Predicate] -> TP [Predicate]
simplify_predicate [] = return []
simplify_predicate ((t `Subsume` t'):p) =
  if t == t'  then simplify_predicate p
              else do p' <- simplify_predicate p
                      return ((t `Subsume` t'):p')
simplify_predicate (Reckind (Record f) l t' : p) =
       simplify_predicate p
simplify_predicate (x:p) =
       do p' <- simplify_predicate p
          return (if x `elem` p' then p' else x : p')




-- assumption, there are only equ predicates

do_unify :: (Subst, [Predicate]) -> TP Subst
do_unify (s, []) = return s
do_unify (s, t `Subsume` t' : p) =
   do s' <- unify(s,t,t')
      do_unify (s',p)
do_unify (s,  _ : p ) = report_error "do_unify: predicate list not in normal form" null_subst



unify :: (Subst, Type, Type) -> TP Subst
unify (s, TV x, TV y) =
   if x == y then return s
   else do t <- apply s (TV x)
           return ((y, t):s)
unify (s, TV x, t) =
            do t'' <- apply s t
               if x `elem` tv t'' then report_error "occurs check fails" null_subst
                else return ((x, t''):s)
unify (s, t, TV x) = unify (s, TV x, t)
unify (s, t1 `Fun` t1', t2 `Fun` t2') = do s' <- unify (s, t1, t2)
                                           unify (s', t1', t2')
unify (s, Record f, Record f') = g (s,f,f') where
          g (ss, [], []) = return ss
          g (ss, (l,t):ff, (l',t'):ff') =
                        if l == l'
                        then do ss' <- unify(ss,t,t')
                                g(ss',ff,ff')
                        else report_error "not matching record" null_subst
          g _ = error "this case was not taken into account in original HM(Rec)"
unify (s, _, _)  = report_error "unify:uncompatible type" null_subst





-- *------------------------------------------
-- * Category: Record instance
-- *------------------------------------------





-- test cases

--test = infer (App (Abs 1 (Id 1)) (Abs 2 (Id 2)))

--test1 = infer (Abs 1 (Abs 2 (Id 1)))

--test2 = infer (Abs 1 (Id 1))

--test3 = infer (Abs 1 (App (Id 1) (Id 1)))

--test4 = infer (Let 1 (Abs 2 (Id 2)) (App (Id 1) (Abs 3 (Id 3))))

--test5 = infer (Let 1 (Abs 2 (Id 2)) (Id 1))

--test6 = infer (Let 1 (Abs 2 (Id 2)) (App (Id 1) (Id 1)))

--test7 = unTP (tp ([(1, Poly [0] (C []) ((TV 0) `Fun` (TV 0)))], App (Id 1) (Id 1))) (init_tvar, null_subst, true_cons)


--rec1 = unTP (tp ([(100, Mono (Record [(200, TV 300),(201, TV 301)])),
--            (101, Poly [302,303] ( C [Reckind (TV 302) 201 (TV 303)]) ((TV 302) `Fun` (TV 303)))],
--           App (Id 101) (Id 100)))
--           (init_tvar, null_subst, true_cons)
rec2, rec3 :: TP (Subst, Constraint)
rec2 = cs(null_subst, C [ Reckind (TV 100) 200 (TV 300 `Fun` TV 300)
                        , Reckind (TV 100) 200 (TV 301)
                        ])

rec3 = cs(null_subst, C [ Reckind (TV 100) 200 (TV 300 `Fun` TV 301)
                        , Reckind (TV 100) 200 (TV 302 `Fun` TV 302)
                        ])
