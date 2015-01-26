{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}


module Luna.Typechecker.Inference (
    tcpass
  ) where


import qualified  Luna.Data.StructInfo                    as SI
import            Luna.Data.StructInfo                    (StructInfo)

import            Luna.Pass                               (Pass(..))

import qualified  Luna.Syntax.Arg                         as Arg
import qualified  Luna.Syntax.Decl                        as Decl
import            Luna.Syntax.Decl                        (LDecl)
import qualified  Luna.Syntax.Enum                        as Enum
import            Luna.Syntax.Enum                        (Enumerated, ID)
import qualified  Luna.Syntax.Expr                        as Expr
import            Luna.Syntax.Expr                        (LExpr)
import qualified  Luna.Syntax.Label                       as Label
import            Luna.Syntax.Label                       (Label(Label), label, element)
import            Luna.Syntax.Module                      (LModule)
import qualified  Luna.Syntax.Name.Pattern                as NamePat
import qualified  Luna.Syntax.Pat                         as Pat
import qualified  Luna.Syntax.Traversals                  as AST

import            Control.Applicative
import            Control.Lens                            hiding (without)
import            Control.Monad.State

import            Data.List
import            Data.Monoid
import qualified  Data.Text.Lazy                          as LText
import            Data.Text.Lazy                          (unpack)
import            Data.Text.Lens                          (packed, unpacked)
import qualified  Data.Foldable                           as Fold

import            Luna.Typechecker.Debug.HumanName        (HumanName(humanName))
import            Luna.Typechecker.Data
import            Luna.Typechecker.StageTypecheckerState  (
                      StageTypecheckerState(..), emptyStageTypecheckerState,
                      debugLog, typo, nextTVar, subst, constr, sa, typeMap,
                      StageTypecheckerState(..), debugLog, typo, nextTVar, subst, constr, sa, currentType,
                      StageTypechecker(..),
                      StageTypecheckerPass, StageTypecheckerCtx,
                      StageTypecheckerTraversal, StageTypecheckerDefaultTraversal,
                      report_error
                  )
import            Luna.Typechecker.Tools                  (without)
import            Luna.Typechecker.TypesAndConstraints
import            Luna.Typechecker.Solver                 (cs)





tcpass :: (StageTypecheckerDefaultTraversal m a) => Pass StageTypecheckerState (a -> StructInfo -> StageTypecheckerPass m StageTypecheckerState)
tcpass = Pass { _name  = "Typechecker"
              , _desc  = "Infers the types and typechecks the program as a form of correctness-proving."
              , _state = emptyStageTypecheckerState
              , _func  = tcUnit
              }

tcUnit :: (StageTypecheckerDefaultTraversal m a) => a -> StructInfo -> StageTypecheckerPass m StageTypecheckerState
tcUnit ast structAnalysis = do
    sa .= structAnalysis
    debugPush "First!"
    _ <- defaultTraverseM ast
    debugLog %= reverse
    get


instance (StageTypecheckerCtx lab m a) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LDecl lab a)    (LDecl lab a)   where traverseM _ = tcDecl
instance (StageTypecheckerCtx lab m a) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LExpr lab a)    (LExpr lab a)   where traverseM _ = tcExpr

traverseM :: (StageTypecheckerTraversal m a) => a -> StageTypecheckerPass m a
traverseM = AST.traverseM StageTypechecker

defaultTraverseM :: (StageTypecheckerDefaultTraversal m a) => a -> StageTypecheckerPass m a
defaultTraverseM = AST.defaultTraverseM StageTypechecker




---- top-level program

--infer :: Term -> E (TVar, Subst, Constraint, Type)
--infer e = unTP (tp (init_typo, e)) (init_tvar, null_subst, true_cons)
----


withTypo ::  (StageTypecheckerCtx lab m a) => Typo -> x lab a -> (x lab a -> StageTypecheckerPass m (x lab a)) -> StageTypecheckerPass m (x lab a)
withTypo typeEnv astElem action = push *> action astElem <* pop
  where
    push = typo %= (typeEnv:)
    pop  = typo %= tail       -- TODO [kgdk] 22 sty 2015: probable cause of problems in the future

tcDecl :: (StageTypecheckerCtx lab m a) => LDecl lab a -> StageTypecheckerPass m (LDecl lab a)
tcDecl ldecl@(Label lab decl) = do
    case decl of
      Decl.Func (Decl.FuncDecl path sig funcout body) -> do
          let baseName = sig ^.  NamePat.base . NamePat.segmentBase . unpacked
              baseArgs = sig ^.. NamePat.base . NamePat.segmentArgs . traverse . Arg.pat . Label.element . to humanName . unpacked
              --   The       |   |              |                     |          |         |               |              |
              --  lens       |   |              |                     |          |         |               |              +-- convert Lazy Text to String
              --  magic      |   |              |                     |          |         |               +-- get human readable form
              --   is        |   |              |                     |          |         +-- skip label, get the element
              --  here.      |   |              |                     |          +-- read the pattern
              -- Behold!     |   |              |                     +-- for each of them
              --             |   |              +-- get the list of segments (ie. list of arguments)
              --             |   +-- get the signature of the function
              --             +-- return the list; if (^.) was here then this would be merged with mappend
          debugPush $ "Function: " ++ baseName ++ " :: (" ++ intercalate ", " baseArgs ++ ") → ???"
          --tp (env, Abs x e) = do a <- newtvar
          --                       b <- tp (insert env (x, Mono (TV a)), e)
          --                       normalize ((TV a) `Fun` b)
      _ -> return ()
    defaultTraverseM ldecl


tcExpr :: (StageTypecheckerCtx lab m a) => LExpr lab a -> StageTypecheckerPass m (LExpr lab a)
tcExpr lexpr@(Label lab expression) = do
    env <- getEnv
    result <- expr env lexpr
    currentType .= result
    return lexpr


expr env (Label lab (Expr.Var { Expr._ident = (Expr.Variable vname _) })) =
  do
    let hn = unpack . humanName $ vname
    hn_id <- getTargetIDString lab
    debugPush ("Var         " ++ hn ++ hn_id)

    targetLabel <- getTargetID lab

    vType <- inst env targetLabel
    result <- normalize vType

    debugPush ("         :: " ++ show result)

    return result

expr env (Label lab (Expr.Assignment { Expr._dst = (Label labt dst), Expr._src = (Label labs src) })) =
  case (dst, src) of
      (Pat.Var { Pat._vname = dst_vname }, Expr.Var { Expr._ident = (Expr.Variable src_vname _) }) ->
        do  
          --tp (env, Let x e e') = do a <- tp (env, e)
          --                          b <- gen env a
          --                          tp ((insert env (x, b)), e')
          t_id <- getTargetIDString labt
          s_id <- getTargetIDString labs
          debugPush ("Assignment  " ++ unpack (humanName dst_vname) ++ t_id ++ " ⬸ " ++ unpack (humanName src_vname) ++ s_id) 
          -- TODO destType <- expr env dst
          TV <$> newtvar 
      _ -> do
            debugPush "Some assignment..."
            TV <$> newtvar
       

expr env ( Label lab ( Expr.App ( NamePat.NamePat { NamePat._base = ( NamePat.Segment appExpr args ) } ) ) ) =
  do
    debugPush "Infering an application..."

    e1Type <- expr env appExpr
    result <- Fold.foldlM tp e1Type args

    debugPush $ "Result of infering an application: " ++ show result

    return result

  where
    tp result arg = do
      let Expr.AppArg _ argV = arg
      argType <- expr env argV
      a <- newtvar
      add_constraint (C [result `Subsume` (argType `Fun` TV a)])
      normalize (TV a)
      -- TODO add mapping between labels -> type to pass's state

expr env (Label lab (Expr.App (NamePat.NamePat { NamePat._base = (NamePat.Segment (Label labb (Expr.Var { Expr._ident = (Expr.Variable basename _)})) args)}))) =
  do
    --tp (env, App e e') = do a <- newtvar
    --                        t <- tp (env, e)
    --                        t' <- tp (env, e')
    --                        add_constraint (C [t `Subsume` (t' `Fun` TV a)])
    --                        normalize (TV a)
    base_id <- getTargetIDString labb
    args_id <- unwords <$> mapM mapArg args
    debugPush ("Application " ++ (unpack . humanName $ basename) ++ base_id ++ " ( " ++ args_id ++ " )")  
    TV <$> newtvar
  where
    mapArg :: (StageTypecheckerCtx lab m a) => Expr.AppArg (LExpr lab a) -> StageTypecheckerPass m String
    mapArg (Expr.AppArg _ (Label laba (Expr.Var { Expr._ident = (Expr.Variable vname _) } ))) = do
        arg_id <- getTargetIDString laba
        return $ (unpack . humanName $ vname) ++ arg_id
    mapArg _ = fail "Luna.Typechecker.Inference:tcExpr:mapArg: usage unexpected"

expr _ _ = error "No idea how to infere type at the moment."


debugPush :: (Monad m) => String -> StageTypecheckerPass m ()
debugPush s = s `seq` debugLog %= (s:)


getTargetIDString :: (StageTypecheckerCtx lab m String) => lab -> StageTypecheckerPass m String
getTargetIDString lab = do
    labtID <- getTargetID lab
    return $ "|" ++ show labID ++ "⊳" ++ show labtID ++ "⊲"
  where
    labID = Enum.id lab


getTargetID :: (StageTypecheckerCtx lab m String) => lab -> StageTypecheckerPass m ID
getTargetID lab =
    sa . SI.alias . ix labID . SI.target & preuse >>= \case
        Nothing     -> return labID
        Just labtID -> return labtID
  where
    labID = Enum.id lab

getEnv :: (Monad m) => StageTypecheckerPass m Typo
getEnv =
    typo & use >>= \case
        []    -> return []
        (x:_) -> return x



-- TODO [kgdk] 22 sty 2015: Constraint should be a monoid
add_cons :: Constraint -> Constraint -> Constraint
add_cons (C p1) (C p2)               = C (p1 ++ p2)
add_cons (C p1) (Proj tvr p2)        = Proj tvr (p1 ++ p2)
add_cons (Proj tvr p1) (C p2)        = Proj tvr (p1 ++ p2)
add_cons (Proj tv1 p1) (Proj tv2 p2) = Proj (tv1 ++ tv2) (p1 ++ p2)


tv_typo :: Typo -> [TVar]
tv_typo = foldl f []
  where
    f z (v,ts) = z ++ tv ts


add_constraint :: (Monad m) => Constraint -> StageTypecheckerPass m ()
add_constraint c1 =
    constr %= (`add_cons` c1)


newtvar :: (Monad m) => StageTypecheckerPass m TVar
newtvar = use nextTVar <* (nextTVar += 1)


insert :: Typo -> (Var, TypeScheme) -> Typo
insert a (x,t) = (x,t):a


rename :: (Monad m) => StageTypecheckerPass m Subst -> TVar ->  StageTypecheckerPass m Subst
rename s x = do
    newtv <- newtvar
    s' <- s
    return ((x, TV newtv):s')


inst :: (Monad m) => Typo -> Var -> StageTypecheckerPass m Type
inst env x =
    case lookup x env of 
        Just ts -> case ts of
            Mono t        ->
                return t
            Poly tvl c t  ->
              do
                s' <- foldl rename (return null_subst) tvl
                c' <- apply s' c
                t' <- apply s' t
                add_constraint c'
                return t'
        Nothing ->
          do
            ntv <- newtvar
            report_error "undeclared variable" (TV ntv)


gen :: (Monad m) =>  Typo -> Type -> StageTypecheckerPass m TypeScheme
gen env t = do
    c      <- use constr
    constr .= projection c (fv t c env)
    return  $ Poly (fv t c env) c t
  where
    fv t1 c1 env1 = without (tv t1 ++ tv c1) (tv_typo env1)


normalize :: (Monad m) => Type ->  StageTypecheckerPass m Type
normalize a = do s <- use subst
                 c <- use constr
                 (s',c') <- cs (s,c)
                 t <- apply s' a
                 return_result s' c' t


return_result :: (Monad m) =>  Subst -> Constraint -> Type ->  StageTypecheckerPass m Type
return_result s c t = do
    subst  .= s
    constr .= c
    return t


projection :: Constraint -> [TVar] -> Constraint
projection _ _ = true_cons

