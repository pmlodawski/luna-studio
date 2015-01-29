> module Main where

*------------------------------------------------
* Category: DECLARATIONS
*------------------------------------------------


The type language

> type TVar       = Int 
> type Var        = Int
> data Type       = TV TVar 
>                 | Type `Fun` Type 
>                     deriving Show

The constraint language

> data Predicate  = TRUE
>                 | Type `Subsume` Type
>                     deriving Show

> data Constraint = C [Predicate]
>                 | Proj [TVar] [Predicate]
>                      deriving Show



Type schemes

> data TypeScheme = Mono Type
>                 | Poly [TVar] Constraint Type
>                      deriving Show

Substitutions and type environments

> type Subst      = [(TVar, Type)]

> type Typo       = [(Var,TypeScheme)]

The term language

> data Term       = Id Var | Abs Var Term | App Term Term | Let Var Term Term
>                      deriving Show

> data E a        = Suc a | Err String
>                      deriving Show

The type inference monad - a typing problem consists of a three tupel

> data TP a = TP ( (TVar, Subst, Constraint) -> E (TVar, Subst, Constraint, a))


*----------------------------------------
* Category: CLASS DECLARATIONS
*----------------------------------------


> class TypesAndConstraints c where
>  apply :: Subst -> c -> c
>  tv    :: c -> [TVar]


*-----------------------------------------
* Category: INSTANCE DECLARATIONS
*-----------------------------------------


> instance TypesAndConstraints Predicate where
>   apply s TRUE              = TRUE
>   apply s (t1 `Subsume` t2) = (apply s t1) `Subsume` (apply s t2)
>   tv TRUE                   = []
>   tv (t1 `Subsume` t2)      = (tv t1) ++ (tv t2)

> instance TypesAndConstraints c => TypesAndConstraints [c] where
>   apply s  = map (apply s) 
>   tv  a    = foldl f [] a where
>                        f z x = z ++ (tv x)

> instance TypesAndConstraints Constraint where
>      apply s (C p)            = C (apply s p)
>      apply s (Proj tvl p)      = Proj tvl (apply s p)
>      tv (C p)                 = tv p
>      tv (Proj tvl p)           = (tv p) -- tvl


> instance TypesAndConstraints Type where
>   apply s (TV tvl)           = case lookup tvl s of
>                                   Just t  -> t
>                                   Nothing -> TV tvl
>   apply s (t1 `Fun` t2)     = (apply s t1) `Fun` (apply s t2)
>   tv (TV tvl)                = [tvl]
>   tv (t1 `Fun` t2)          = (tv t1) ++ (tv t2)


> instance TypesAndConstraints TypeScheme where
>   apply s (Poly tvl c t)     = Poly tvl (apply s c) (apply s t)
>   apply s (Mono t)          = Mono (apply s t)                  
>   tv (Poly tvl c t)          = ((tv t) ++ (tv c)) -- tvl
>   tv (Mono t)               = tv t


*--------------------------------------------------
* Category: MONAD DECLARATIONS
*--------------------------------------------------


> unTP (TP a) = a


> instance Monad TP where
>  m >>= k = TP ( \ (n,s,c) -> 
>                      case unTP (m) (n,s,c) of 
>                        Suc (n',s',c',x) -> unTP (k x) (n',s',c')
>                        Err s            -> Err s )
>  return x = TP ( \ (n,s,c) -> return (n,s,c,x) )


> instance Monad E where
>  m >>= k = case m of
>            Suc a -> k a
>            Err s -> Err s
>  return x = Suc x


*---------------------------------------
* Category: INITIALIZATIONS
*---------------------------------------


> null_subst :: Subst
> null_subst = []

> init_typo :: Typo

FILL IN initial type environment

> true_cons :: Constraint
> true_cons = C [TRUE]

> init_tvar :: TVar
> init_tvar = 0

> new_tvar :: TVar -> TVar
> new_tvar n = n + 1



*--------------------------------------
* Category: Core type inferencer
*--------------------------------------



primitives for dealing with constraints

> add_cons :: Constraint -> Constraint -> Constraint
> add_cons (C p1) (C p2)               = C (p1 ++ p2)
> add_cons (C p1) (Proj tv p2)         = Proj tv (p1 ++ p2)
> add_cons (Proj tv p1) (C p2)         = Proj tv (p1 ++ p2)
> add_cons (Proj tv1 p1) (Proj tv2 p2) = Proj (tv1 ++ tv2) (p1 ++ p2)   


> projection :: Constraint -> [TVar] -> Constraint

 FILL IN DEFINITION

 projection might vary depending on different kinds of constraint systems


 projection (C p) tvl         = Proj tvl p
 projection (Proj tv1 p) tv2 = Proj (tv1 ++ tv2) p 


lifted functions


> tv_typo :: Typo -> [TVar]
> tv_typo env = foldl f [] env where
>           f z (v,ts) = z ++ (tv ts)

> add_constraint :: Constraint -> TP ()
> add_constraint c1 =
>      TP (\ (n,s,c) -> return (n,s,add_cons c c1,()))



handling of type variables and type environments

> newtvar :: TP TVar
> newtvar = TP (\ (n,s,c) -> return (new_tvar n,s,c,n) )

> insert :: Typo -> (Var, TypeScheme) -> Typo
> insert a (x,t) = (x,t):a

> mylookup :: Typo -> Var -> E TypeScheme
> mylookup [] y =
>     Err "undeclared variable"
> mylookup ((x,t):xs) y =
>       if x == y then return t
>       else mylookup xs y 



instantiation and generalization



> rename :: [TVar] -> Int-> (Int, Subst)
> rename [] n = (n, null_subst)
> rename (x:r) n = let (n', s) = rename r n
>                  in (n'+1, (x,TV n'):s)

> inst :: Typo -> Var -> TP Type
> inst env x = 
>   TP ( \ (n,s,c) -> case mylookup env x of
>                        Suc ts -> case ts of
>                                   Mono t       -> return (n,s,c,t)
>                                   Poly tvl c' t -> let (n', s') = rename tvl n
>                                                    in return (n',s, add_cons (apply s' c') c, apply s' t)
>                        Err _  -> Err "undeclared variable" )




> gen :: Typo -> Type -> TP TypeScheme
> gen env t =
>  TP ( \ (n,s,c) -> return (n,s, projection c (fv t c env), Poly (fv t c env) c t) ) where fv t1 c1 env1 = ((tv t1) ++ (tv c1)) -- (tv_typo env1)
>


constraint solver


> cs :: (Subst, Constraint) -> E (Subst, Constraint)


that's what you need to supply


incorporating the constraint solver into monad TP

> normalize :: Type -> TP Type
> normalize a =
>   TP ( \ (n,s,c) -> do (s',c') <- cs (s,c)
>                        return (n,s',c', apply s' a))

type inference

> tp :: (Typo, Term) -> TP Type
> tp (env, Id x) =  do a <- inst env x
>                      normalize a
       
> tp (env, Abs x e) = do a <- newtvar
>                        b <- tp (insert env (x, Mono (TV a)), e)
>                        normalize ((TV a) `Fun` b)

> tp (env, App e e') = do a <- newtvar
>                         t <- tp (env, e)
>                         t' <- tp (env, e')
>                         add_constraint (C [t `Subsume` (t' `Fun` TV a)])
>                         normalize (TV a)


> tp (env, Let x e e') = do a <- tp (env, e)
>                           b <- gen env a
>                           tp ((insert env (x, b)), e')

top-level program

> infer :: Term -> E (TVar, Subst, Constraint, Type)
> infer e = unTP (tp (init_typo, e)) (init_tvar, null_subst, true_cons)
         



*------------------------------------------
* Category: Hindley/Milner instance
*------------------------------------------



> init_typo = []

Invariant: all constraints are true, hence projection is always trivial

> projection _ _ = true_cons


> occurs :: (Type, Type) -> Bool
> occurs (TV x, t) = elem x v where
>                      v = f t where
>                              f (TV a) = [a]
>                              f (b `Fun` b') = (f b) ++ (f b')
> occurs (t `Fun` t', t'') =
>  (occurs (t, t'')) || (occurs (t', t''))


> unify :: (Subst, Type, Type) -> E Subst
> unify (s, TV x, TV y) =
>    if x == y then return s
>    else Suc f where
>       f = (y, apply s (TV x)):s
> unify (s, TV x, t `Fun` t') =
>    if occurs (TV x, (apply s (t `Fun` t'))) then Err "fail occurs check"
>    else Suc f where
>         f = (x, apply s (t `Fun` t')):s
> unify (s, t `Fun` t', TV x) = unify (s, TV x, t `Fun` t')
> unify (s, t1 `Fun` t1', t2 `Fun` t2') =
>    case unify (s, t1, t2) of
>       Suc s'   -> unify (s', t1', t2')
>       Err fail -> Err fail


> cs (s, C ((t `Subsume` t'):c)) = do s' <- unify (s,t,t')
>                                     cs  (s', apply s' (C c))
> cs (s, C (TRUE:c))         = cs (s, C c)
> cs (s, C [])               = return (s, true_cons) 


test cases

> test = infer (App (Abs 1 (Id 1)) (Abs 2 (Id 2)))

> test1 = infer (Abs 1 (Abs 2 (Id 1)))

> test2 = infer (Abs 1 (Id 1))

> test3 = infer (Abs 1 (App (Id 1) (Id 1)))

> test4 = infer (Let 1 (Abs 2 (Id 2)) (App (Id 1) (Abs 3 (Id 3))))

> test5 = infer (Let 1 (Abs 2 (Id 2)) (Id 1))

> test6 = infer (Let 1 (Abs 2 (Id 2)) (App (Id 1) (Id 1)))

 test7 = tp ([(1, Poly [0] [] ((TV 0) `Fun` (TV 0)))], App (Id 1) (Id 1)) (init_tvar, init_subst, init_cons)



