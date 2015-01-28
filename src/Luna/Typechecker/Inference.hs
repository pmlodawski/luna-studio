{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}


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
import            Luna.Syntax.Enum                        (Enumerated, ID, IDTag(IDTag))
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

import            Data.Default                            (Default(def))
import            Data.List                               hiding (insert)
import            Data.Maybe                              (fromMaybe)
import            Data.Monoid
import qualified  Data.Text.Lazy                          as LText
import            Data.Text.Lazy                          (unpack)
import            Data.Text.Lens                          (packed, unpacked)
import qualified  Data.Foldable                           as Fold

import            Luna.Typechecker.Debug.HumanName        (HumanName(humanName))
import            Luna.Typechecker.Data
import            Luna.Typechecker.StageTypecheckerState  (
                      StageTypecheckerState(..),
                      debugLog, typo, nextTVar, subst, constr, sa, currentType, typeMap,
                      StageTypechecker(..),
                      StageTypecheckerPass, StageTypecheckerCtx, StageTypecheckerTraversal, StageTypecheckerDefaultTraversal,
                      InExpr, OutExpr,
                      report_error
                  )
import            Luna.Typechecker.Tools                  (without)
import            Luna.Typechecker.TypesAndConstraints
import            Luna.Typechecker.Solver                 (cs)





tcpass :: (StageTypecheckerDefaultTraversal m a) => Pass StageTypecheckerState (a -> StructInfo -> StageTypecheckerPass m (a, StageTypecheckerState))
tcpass = Pass { _name  = "Typechecker"
              , _desc  = "Infers the types and typechecks the program as a form of correctness-proving."
              , _state = def
              , _func  = tcUnit
              }

tcUnit :: (StageTypecheckerDefaultTraversal m a) => a -> StructInfo -> StageTypecheckerPass m (a, StageTypecheckerState)
tcUnit ast structAnalysis = do
    sa .= structAnalysis
    debugPush "First!"
    liftM2 (,) (defaultTraverseM ast) get


instance (StageTypecheckerCtx IDTag m) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LDecl IDTag InExpr) (LDecl IDTag InExpr) where
  traverseM _ = tcDecl
instance (StageTypecheckerCtx IDTag m) => AST.Traversal StageTypechecker (StageTypecheckerPass m) (LExpr IDTag ()) (LExpr IDTag ()) where
  traverseM _ = tcExpr

traverseM :: (StageTypecheckerTraversal m a) => a -> StageTypecheckerPass m a
traverseM = AST.traverseM StageTypechecker

defaultTraverseM :: (StageTypecheckerDefaultTraversal m a) => a -> StageTypecheckerPass m a
defaultTraverseM = AST.defaultTraverseM StageTypechecker




---- top-level program

--infer :: Term -> E (TVar, Subst, Constraint, Type)
--infer e = unTP (tp (init_typo, e)) (init_tvar, null_subst, true_cons)
----


withTypo :: (Monad m) => Typo -> a -> (a -> StageTypecheckerPass m b) -> StageTypecheckerPass m b
withTypo typeEnv astElem action = push *> action astElem <* pop
  where
    push        = typo %= (typeEnv:)
    pop         = typo %= safeTail
    safeTail xs = fromMaybe xs (xs ^? _tail)


withClonedTypo :: (Monad m) => a -> (a -> StageTypecheckerPass m b) -> StageTypecheckerPass m b
withClonedTypo x action = do
  typo0 <- typo . _head & use
  withTypo typo0 x action


withClonedTypo0 :: (Monad m) => StageTypecheckerPass m a -> StageTypecheckerPass m a
withClonedTypo0 = withClonedTypo () . const


--tcDecl :: (StageTypecheckerCtx lab m) => LDecl lab InExpr -> StageTypecheckerPass m (LDecl lab OutExpr)
tcDecl ldecl@(Label lab decl) =
    case decl of
      Decl.Func (Decl.FuncDecl path sig funcout body) -> do
          let labID       = Enum.id lab
              baseName    = sig ^.  NamePat.base . NamePat.segmentBase . unpacked
              argumentIDEs= sig ^.. NamePat.base . NamePat.segmentArgs . traverse . Arg.pat . Label.label
              argumentIDs = sig ^.. NamePat.base . NamePat.segmentArgs . traverse . Arg.pat . Label.label   . to Enum.id
              baseArgs    = sig ^.. NamePat.base . NamePat.segmentArgs . traverse . Arg.pat . Label.element . to humanName . unpacked
              bodyIDs     = sig ^.. NamePat.base . NamePat.segmentBase

          labIDdisp <- getTargetIDString lab
          bsIDs <- forM argumentIDEs getTargetIDString
          let argsDisp = zipWith (++) baseArgs bsIDs


          debugPush "push"
          withClonedTypo0 $ do

              debugPush $ "Function: " ++ baseName ++ labIDdisp ++ " :: (" ++ intercalate ", " argsDisp ++ ") → ???"

              a  <- insertNewMonoTypeVariable (Enum.id lab)     -- type of the method
              bs <- forM argumentIDs insertNewMonoTypeVariable  -- types of the arguments
              
              travRes <- defaultTraverseM ldecl
              
              mResultType <- typeMap . at labID & use

              case mResultType of
                Just resultType -> do
                    let bsres = foldr1 Fun (bs ++ [resultType])

                    add_constraint (C [a `Subsume` bsres])
                    typ <- normalize a
                    -- TODO [kgdk] 27 sty 2015: generalize
                    typeMap . at labID ?= typ

                    debugPush "pop"
                    return travRes
                Nothing -> do
                    mm <- typeMap & use
                    debugPush $ "PRE ERR: " ++ show mm
                    report_error ("no type returned for " ++ show labID) travRes
          
          --tp (env, Abs x e) = do a <- newtvar
          --                       b <- tp (insert env (x, Mono (TV a)), e)
          --                       normalize ((TV a) `Fun` b)
      _ -> defaultTraverseM ldecl


insertNewMonoTypeVariable :: (Monad m) => ID -> StageTypecheckerPass m Type
insertNewMonoTypeVariable labID = do
    tvarID <- newtvar
    let typeEnvElem = (labID, Mono (TV tvarID))
    typo . _head %= flip insert typeEnvElem
    return $ TV tvarID


--tcExpr :: (StageTypecheckerCtx IDTag m) => LExpr IDTag () -> StageTypecheckerPass m (LExpr IDTag ())
tcExpr lexpr@(Label lab _) = do
  labID <- getTargetIDString lab
  debugPush $ "tcExpr: " ++ labID
  expr lexpr <* debugPush ("tcExpr: " ++ labID ++ " DONE")


expr :: (StageTypecheckerCtx IDTag m) => LExpr IDTag () -> StageTypecheckerPass m (LExpr IDTag ())
expr var@(Label lab (Expr.Var { Expr._ident = (Expr.Variable vname _) })) = 
  do
    let hn = unpack . humanName $ vname
    hn_id <- getTargetIDString lab
    debugPush ("Var         " ++ hn ++ hn_id)

    targetLabel <- getTargetID lab
    env <- getEnv
    vType <- inst targetLabel
    result <- normalize vType

    debugPush ("         :: " ++ show result)

    setTypeById targetLabel result

    defaultTraverseM var


expr ass@(Label lab (Expr.Assignment { Expr._dst = (Label labt dst), Expr._src = (Label labs src) })) = do
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
      _ -> do
            debugPush "Some assignment..."
  
  defaultTraverseM ass

expr app@( Label lab ( Expr.App ( NamePat.NamePat { NamePat._base = ( NamePat.Segment appExpr args ) } ) ) ) =
  do
    debugPush "Infering an application..."

    let Label appExprLab _ = appExpr
    appId <- getTargetID lab
    appExprId <- getTargetID appExprLab

    -- typecheck all args
    res <- defaultTraverseM app

    e1Type <- getTypeById appExprId
    let argTypes = map toType args
    result <- Fold.foldlM tp e1Type argTypes
    setTypeById appId result

    debugPush $ "Result of infering an application: " ++ show result
    return res

  where
    toType (Expr.AppArg _ (Label lab _)) = do
      getTypeById =<< getTargetID lab

    tp result arg = do
      argType <- arg
      a <- newtvar
      add_constraint (C [result `Subsume` (argType `Fun` TV a)])
      normalize (TV a)

--expr _ = error "No idea how to infer type at the moment."
expr _ = error "inny error"

getTypeById :: Monad m => ID -> StageTypecheckerPass m Type
getTypeById idV = do
    typeResult <- typeMap . at idV & use
    maybe (error "Can't find type using id.") (return . id) typeResult


setTypeById id typeV = do
    debugPush $ "save " ++ show id ++ " ?= " ++ show typeV
    typeMap . at id ?= typeV
    mm <- typeMap & use
    debugPush $ show mm


debugPush :: (Monad m) => String -> StageTypecheckerPass m ()
debugPush s = s `seq` debugLog %= (s:)


--getTargetIDString :: (StageTypecheckerCtx lab m) => lab -> StageTypecheckerPass m String
getTargetIDString lab = do
    labtID <- getTargetID lab
    return $ "|" ++ show labID ++ "⊳" ++ show labtID ++ "⊲"
  where
    labID = Enum.id lab


--getTargetID :: (StageTypecheckerCtx lab m) => lab -> StageTypecheckerPass m ID
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



---- TODO [kgdk] 22 sty 2015: Constraint should be a monoid
add_cons :: Constraint -> Constraint -> Constraint
add_cons (C p1) (C p2)               = C (p1 ++ p2)
add_cons (C p1) (Proj tvr p2)        = Proj tvr (p1 ++ p2)
add_cons (Proj tvr p1) (C p2)        = Proj tvr (p1 ++ p2)
add_cons (Proj tv1 p1) (Proj tv2 p2) = Proj (tv1 ++ tv2) (p1 ++ p2)


--tv_typo :: Typo -> [TVar]
--tv_typo = foldl f []
--  where
--    f z (v,ts) = z ++ tv ts


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


inst :: (Monad m) => Var -> StageTypecheckerPass m Type
inst x = do
    env <- getEnv
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
            report_error ("undeclared variable " ++ show x) (TV ntv)


--gen :: (Monad m) =>  Typo -> Type -> StageTypecheckerPass m TypeScheme
--gen env t = do
--    c      <- use constr
--    constr .= projection c (fv t c env)
--    return  $ Poly (fv t c env) c t
--  where
--    fv t1 c1 env1 = without (tv t1 ++ tv c1) (tv_typo env1)


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


--projection :: Constraint -> [TVar] -> Constraint
--projection _ _ = true_cons

