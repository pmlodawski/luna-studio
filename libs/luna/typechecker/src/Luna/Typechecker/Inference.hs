{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}


-- *------------------------------------------------
-- * Specification of the generic HM(X)
-- * type inference system in Haskell
-- *
-- * This instance deals with Ohori style records
-- *------------------------------------------------


module Luna.Typechecker.Inference where

import            Luna.Pass                               (PassMonad, PassCtx, Pass(Pass))
import qualified  Luna.ASTNew.Decl                        as Decl
import            Luna.ASTNew.Decl                        (LDecl)
import qualified  Luna.ASTNew.Enum                        as Enum
import            Luna.ASTNew.Enum                        (Enumerated)
import qualified  Luna.ASTNew.Expr                        as Expr
import            Luna.ASTNew.Expr                        (LExpr)
import qualified  Luna.ASTNew.Label                       as Label
import            Luna.ASTNew.Label                       (Label(Label))
import qualified  Luna.ASTNew.Module                      as Module
import            Luna.ASTNew.Module                      (LModule)
import qualified  Luna.ASTNew.Name.Pattern                as NamePat
import qualified  Luna.ASTNew.Pat                         as Pat
import qualified  Luna.ASTNew.Traversals                  as AST
import qualified  Luna.Data.StructInfo                    as SI
import            Luna.Data.StructInfo                    (StructInfo)

import            Control.Applicative
import            Control.Lens                            hiding (without)
import            Control.Monad.State
import            Data.List                               (intercalate)
import            Data.Text.Lazy                          (unpack)
import qualified  Text.PrettyPrint                        as PP
import            Text.PrettyPrint                        (($+$),(<+>), (<>))

import            Luna.Typechecker.Debug.HumanName        (HumanName(humanName))
import            Luna.Typechecker.Data                   (TVar, Var, Fieldlabel, Field, Subst, Typo, Type(..), Predicate(..), Constraint(..), TypeScheme(..))
import            Luna.Typechecker.StageTypecheckerState  (StageTypecheckerState(..), str, typo, nextTVar, subst, constr, sa)





tcpass :: (StageTypecheckerDefaultTraversal m a) => Pass StageTypecheckerState (a -> StructInfo -> StageTypecheckerPass m StageTypecheckerState)
tcpass = Pass "Typechecker"
              "Infers the types and typechecks the program as a form of correctness-proving."
              StageTypecheckerState { _str      = []
                                    , _typo     = []
                                    , _nextTVar = 0
                                    , _subst    = []
                                    , _constr   = C [TRUE]
                                    }
              tcUnit

tcUnit :: (StageTypecheckerDefaultTraversal m a) => a -> StructInfo -> StageTypecheckerPass m StageTypecheckerState
tcUnit ast structAnalysis =
  do
    sa .= structAnalysis
    pushString "First!"
    _ <- defaultTraverseM ast
    str %= reverse
    get


data StageTypechecker = StageTypechecker

type StageTypecheckerPass             m       = PassMonad StageTypecheckerState m
type StageTypecheckerCtx              lab m a = (HumanName (Pat.Pat lab), Enumerated lab, StageTypecheckerTraversal m a)
type StageTypecheckerTraversal        m   a   = (PassCtx m, AST.Traversal        StageTypechecker (StageTypecheckerPass m) a a)
type StageTypecheckerDefaultTraversal m   a   = (PassCtx m, AST.DefaultTraversal StageTypechecker (StageTypecheckerPass m) a a)

instance (StageTypecheckerCtx lab m a) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LModule lab a)  (LModule lab a) where traverseM _ = tcMod
instance (StageTypecheckerCtx lab m a) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LDecl lab a)    (LDecl lab a)   where traverseM _ = tcDecl
instance (StageTypecheckerCtx lab m a) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LExpr lab a)    (LExpr lab a)   where traverseM _ = tcExpr


traverseM :: (StageTypecheckerTraversal m a) => a -> StageTypecheckerPass m a
traverseM = AST.traverseM StageTypechecker

defaultTraverseM :: (StageTypecheckerDefaultTraversal m a) => a -> StageTypecheckerPass m a
defaultTraverseM = AST.defaultTraverseM StageTypechecker


tcMod :: (StageTypecheckerCtx lab m a) => LModule lab a -> StageTypecheckerPass m (LModule lab a)
tcMod lmodule@(Label _ Module.Module {Module._path = path, Module._name = name, Module._body = body} ) =
  do
    pushString ("Module      " ++ intercalate "." (fmap unpack (path ++ [name])))
    defaultTraverseM lmodule


tcDecl :: (StageTypecheckerCtx lab m a) => LDecl lab a -> StageTypecheckerPass m (LDecl lab a)
tcDecl ldecl@(Label lab decl) =
    case decl of
        fun@Decl.Func { Decl._sig  = sig@NamePat.NamePat{ NamePat._base = (NamePat.Segment name args) }
                      , Decl._body = body
                      } ->
          do  
            name_ids <- getTargetID lab
            args_ids <- unwords <$> mapM mapArg args
            pushString ("Function    " ++ unpack name ++ name_ids ++ " " ++ args_ids ++ " START")
            x <- defaultTraverseM ldecl
            pushString ("Function    " ++ unpack name ++ name_ids ++ " " ++ args_ids ++ " END") 
            return x
        _ ->
            defaultTraverseM ldecl
  where mapArg :: (Enumerated lab, Monad m) => NamePat.Arg (Pat.LPat lab) a -> StageTypecheckerPass m String
        mapArg (NamePat.Arg (Label lab arg) _) =
          do
            arg_id <- getTargetID lab
            return $ unpack (humanName arg) ++ arg_id


tcExpr :: (StageTypecheckerCtx lab m a) => LExpr lab a -> StageTypecheckerPass m (LExpr lab a)
tcExpr lexpr@(Label lab expr) =
  do
    case expr of 
        Expr.Var { Expr._ident = (Expr.Variable vname _) } ->
          do
            let hn = unpack . humanName $ vname
            hn_id <- getTargetID lab
            pushString ("Var         " ++ hn ++ hn_id)
        Expr.Assignment { Expr._dst = (Label labt dst), Expr._src = (Label labs src) } ->

            case (dst, src) of
                (Pat.Var { Pat._vname = dst_vname }, Expr.Var { Expr._ident = (Expr.Variable src_vname _) }) ->
                  do  
                    t_id <- getTargetID labt
                    s_id <- getTargetID labs
                    pushString ("Assignment  " ++ unpack (humanName dst_vname) ++ t_id ++ " ⬸ " ++ unpack (humanName src_vname) ++ s_id) 
                _ -> pushString "Some assignment..."
        Expr.App (NamePat.NamePat { NamePat._base = (NamePat.Segment (Label labb (Expr.Var { Expr._ident = (Expr.Variable basename _)})) args)}) ->
          do
            base_id <- getTargetID labb
            args_id <- unwords <$> mapM mapArg args
            pushString ("Application " ++ (unpack . humanName $ basename) ++ base_id ++ " ( " ++ args_id ++ " )")
        _ ->
            return ()
    defaultTraverseM lexpr
  where mapArg :: (Enumerated lab, Monad m) => Expr.AppArg (LExpr lab a) -> StageTypecheckerPass m String
        mapArg (Expr.AppArg _ (Label laba (Expr.Var { Expr._ident = (Expr.Variable vname _) } ))) = do
            arg_id <- getTargetID laba
            return $ (unpack . humanName $ vname) ++ arg_id



pushString :: (Monad m) => String -> StageTypecheckerPass m ()
pushString s = str %= (s:)

getTargetID :: (Enumerated lab, Monad m) => lab -> StageTypecheckerPass m String
getTargetID lab =
  do
    sa . SI.alias . at labID & use >>= \case
        Nothing     -> return $ "|" ++ show labID ++ "⊲"                
        Just labtID -> return $ "|" ++ show labID ++ "⊳" ++ show labtID ++ "⊲"
  where labID = Enum.id lab



---- type inference

--tp :: (Monad m) =>  (Typo, Term) ->  StageTypecheckerPass m Type
--tp (env, Id x) =  do a <- inst env x
--                     normalize a
----
--tp (env, Abs x e) = do a <- newtvar
--                       b <- tp (insert env (x, Mono (TV a)), e)
--                       normalize ((TV a) `Fun` b)

--tp (env, App e e') = do a <- newtvar
--                        t <- tp (env, e)
--                        t' <- tp (env, e')
--                        add_constraint (C [t `Subsume` (t' `Fun` TV a)])
--                        normalize (TV a)


--tp (env, Let x e e') = do a <- tp (env, e)
--                          b <- gen env a
--                          tp ((insert env (x, b)), e')

---- top-level program

--infer :: Term -> E (TVar, Subst, Constraint, Type)
--infer e = unTP (tp (init_typo, e)) (init_tvar, null_subst, true_cons)
----



-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################
-- #############################################################################

-- The error monad

--data E a        = Suc a | Err String
--                     deriving Show

---- The type inference monad - a typing problem consists of a three tuple

--data TP a = TP ( (TVar, Subst, Constraint) ->
--                 E (TVar, Subst, Constraint, a))


-- *----------------------------------------
-- * Category: CLASS DECLARATIONS
-- *----------------------------------------


class TypesAndConstraints c where
 apply :: (Monad m) =>  Subst -> c -> StageTypecheckerPass m c
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

--unTP :: (Monad m) =>   StageTypecheckerPass m t -> (TVar, Subst, Constraint) -> E (TVar, Subst, Constraint, t)
--unTP (TP a) = a


--instance Monad TP where
-- m >>= k = TP ( \ (n,s,c) ->
--                     case unTP m (n,s,c) of
--                       Suc (n',s',c',x) -> unTP (k x) (n',s',c')
--                       Err se           -> Err se )
-- return x = TP ( \ (n,s,c) -> return (n,s,c,x) )


--instance Functor TP where
--  fmap f m = TP $ \(n,s,c) ->
--                     case unTP m (n,s,c) of
--                       Suc (n',s',c',x) -> Suc (n',s',c',f x)
--                       Err se           -> Err se

--instance Applicative TP where
--  pure = return
--  (<*>) = ap


--instance Monad E where
-- m >>= k = case m of
--           Suc a -> k a
--           Err s -> Err s
-- return  = Suc

--instance Functor E where
--  fmap f m = case m of
--           Suc a -> Suc (f a)
--           Err s -> Err s

--instance Applicative E where
--  pure = return
--  (<*>) = ap


report_error :: (Monad m) =>  String -> a ->  StageTypecheckerPass m a
report_error msg x = fail $ "LUNA ERROR: " ++ msg
-- report_error msg x = TP ( \ (n,s,c) -> Err msg)


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

add_constraint :: (Monad m) => Constraint -> StageTypecheckerPass m ()
add_constraint c1 =
    constr %= flip add_cons c1
     --TP (\ (n,s,c) -> return (n,s,add_cons c c1,()))



-- handling of type variables and type environments

newtvar :: (Monad m) => StageTypecheckerPass m TVar
newtvar = do
    n <- use nextTVar
    nextTVar += 1
    return n
-- newtvar = TP (\ (n,s,c) -> return (new_tvar n,s,c,n) )

insert :: Typo -> (Var, TypeScheme) -> Typo
insert a (x,t) = (x,t):a





-- instantiation and generalization



rename :: (Monad m) =>  (Monad m) => StageTypecheckerPass m Subst -> TVar ->  StageTypecheckerPass m Subst
rename s x = do newtv <- newtvar
                s' <- s
                return ((x, TV newtv):s')

inst :: (Monad m) => Typo -> Var -> StageTypecheckerPass m Type
inst env x =
   case mylookup env x of
     Just ts -> case ts of
                  Mono t        -> return t
                  Poly tvl c t  -> do s' <- foldl rename (return null_subst) tvl
                                      c' <- apply s' c
                                      t' <- apply s' t
                                      add_constraint c'
                                      return t'
     Nothing -> do  ntv <- newtvar
                    report_error "undeclared variable" (TV ntv)
  where
    mylookup :: Typo -> Var -> Maybe TypeScheme
    mylookup [] y = Nothing
    mylookup ((x,t):xs) y =
          if x == y then return t
                    else mylookup xs y




gen :: (Monad m) =>  Typo -> Type -> StageTypecheckerPass m TypeScheme
gen env t = do
    c     <- use constr
    --let c = true_cons
    constr .= projection c (fv t c env)
    return $ Poly (fv t c env) c t
  where
    fv t1 c1 env1 = without (tv t1 ++ tv c1) (tv_typo env1)
-- TP ( \ (n,s,c) -> return (n,s, projection c (fv t c env), Poly (fv t c env) c t) )



-- constraint solver






-- that's what you need to supply


-- incorporating the constraint solver into monad TP

normalize :: (Monad m) =>  Type ->  StageTypecheckerPass m Type
normalize a = do s <- get_subst
                 c <- get_cons
                 (s',c') <- cs (s,c)
                 t <- apply s' a
                 return_result s' c' t



get_subst :: (Monad m) =>   StageTypecheckerPass m Subst
get_subst =
  use subst
--get_subst = TP ( \ (n,s,c) -> return (n,s,c,s))

get_cons :: (Monad m) =>   StageTypecheckerPass m Constraint
get_cons =
  use constr
--get_cons = TP ( \ (n,s,c) -> return (n,s,c,c))

return_result :: (Monad m) =>  Subst -> Constraint -> Type ->  StageTypecheckerPass m Type
return_result s c t = do
  subst  .= s
  constr .= c
  return t
--return_result s c t = TP ( \ (n,s',c') -> return (n,s,c,t))






init_typo = []

-- Invariant: all constraints are true, hence projection is always trivial

projection _ _ = true_cons


cs :: (Monad m) =>  (Subst, Constraint) -> StageTypecheckerPass m (Subst, Constraint)
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

extract_predicates :: (Monad m) =>  [Predicate] ->  StageTypecheckerPass m ([Predicate],[Predicate])
extract_predicates [] = return ([],[])
extract_predicates (TRUE:p) = extract_predicates p
extract_predicates ((t `Subsume` t'):p) =
       do (r,e) <- extract_predicates p
          return (r, (t `Subsume` t'):e)
extract_predicates (Reckind t l t' : p) =
       do (r,e) <- extract_predicates p
          return ( Reckind t l t' : r, e)



closure :: (Monad m) =>   (Subst,[Predicate],[Predicate]) ->  StageTypecheckerPass m (Subst,[Predicate])
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

extract1 :: (Monad m) =>  [Predicate] ->  StageTypecheckerPass m [Predicate]
extract1 [] = return []
extract1 (Reckind (Record f) l t : p) =
    do e <- get_extract1 f l t
       e' <- extract1 p
       return (e:e')
extract1 (_:p) = extract1 p
get_extract1 :: (Monad m) =>  Eq a => [(a, Type)] -> a -> Type ->  StageTypecheckerPass m Predicate
get_extract1 [] _ _          = report_error "extract1:field label not found -> inconsistent constraint" (TV 0 `Subsume` TV 0)
get_extract1 ((l,t):f) l' t' = if l == l' then return (t `Subsume` t')
                               else get_extract1 f l' t'

-- Create subsumptions for each label based on all constraints for a particular label and record. All equations for a particular label in a record must be satisfied

extract2 :: (Monad m) =>  [Predicate] ->  StageTypecheckerPass m [Predicate]
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



check_consistency :: (Monad m) =>  [Predicate] ->  StageTypecheckerPass m Bool
check_consistency [] = return True
check_consistency (Reckind (Record f) l t' : p) = check_consistency p
check_consistency (Reckind (TV a) l t'     : p) = check_consistency p
check_consistency (Reckind _ l t'          : p) = return False
check_consistency (_                       : p) = check_consistency p



-- simplification of constraints

simplify :: (Monad m) =>  Constraint ->  StageTypecheckerPass m Constraint
simplify (C p) = do p' <- simplify_predicate p
                    return (C p')
simplify _ = error "this case was not taken into account in original HM(Rec)"

-- simplification of predicates

simplify_predicate :: (Monad m) =>  [Predicate] ->  StageTypecheckerPass m [Predicate]
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

do_unify :: (Monad m) =>  (Subst, [Predicate]) ->  StageTypecheckerPass m Subst
do_unify (s, []) = return s
do_unify (s, t `Subsume` t' : p) =
   do s' <- unify(s,t,t')
      do_unify (s',p)
do_unify (s,  _ : p ) = report_error "do_unify: predicate list not in normal form" null_subst



unify :: (Monad m) =>  (Subst, Type, Type) ->  StageTypecheckerPass m Subst
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
--rec2, rec3 :: (Monad m) =>   StageTypecheckerPass m (Subst, Constraint)
--rec2 = cs(null_subst, C [ Reckind (TV 100) 200 (TV 300 `Fun` TV 300)
--                        , Reckind (TV 100) 200 (TV 301)
--                        ])

--rec3 = cs(null_subst, C [ Reckind (TV 100) 200 (TV 300 `Fun` TV 301)
--                        , Reckind (TV 100) 200 (TV 302 `Fun` TV 302)
--                        ])