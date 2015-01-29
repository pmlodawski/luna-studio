{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Luna.Typechecker.Inference (
    tcpass
  ) where


import            Control.Applicative
import            Control.Lens                            hiding (without)
import            Control.Monad.State

import            Data.Default                            (Default(def))
import qualified  Data.Foldable                           as Fold
import            Data.List                               (intercalate)
import            Data.Text.Lazy                          (unpack)
import            Data.Text.Lens                          (unpacked)

import            Luna.Data.StructInfo                    (StructInfo)

import            Luna.Pass                               (Pass(..))

import qualified  Luna.Syntax.Arg                         as Arg
import qualified  Luna.Syntax.Decl                        as Decl
import qualified  Luna.Syntax.Enum                        as Enum
import            Luna.Syntax.Enum                        (IDTag)
import qualified  Luna.Syntax.Expr                        as Expr
import            Luna.Syntax.Expr                        (LExpr)
import qualified  Luna.Syntax.Label                       as Label
import            Luna.Syntax.Label                       (Label(Label))
import qualified  Luna.Syntax.Name.Pattern                as NamePat
import qualified  Luna.Syntax.Pat                         as Pat
import qualified  Luna.Syntax.Traversals                  as AST

import            Luna.Typechecker.Data (
                      Var, Subst,
                      Type(..), Predicate(..), Constraint(..), TypeScheme(..),
                      null_subst
                  )
import            Luna.Typechecker.Debug.HumanName        (HumanName(humanName))
import            Luna.Typechecker.Inference.Class        (
                      StageTypechecker(..),
                      StageTypecheckerPass, StageTypecheckerCtx,
                      StageTypecheckerTraversal, StageTypecheckerDefaultTraversal,
                      InExpr, OutExpr, InDecl, OutDecl
                  )
import            Luna.Typechecker.Solver                 (cs)
import            Luna.Typechecker.StageTypecheckerState  (
                      StageTypecheckerState(..),
                      subst, constr, sa, typeMap,
                      report_error,
                      withClonedTypo0,
                      insertNewMonoTypeVariable,
                      getTypeById, setTypeById, getEnv,
                      add_constraint, newtvar, rename,
                      debugPush,
                      getTargetIDString, getTargetID
                  )
import            Luna.Typechecker.TypesAndConstraints    (TypesAndConstraints(..))



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


instance (StageTypecheckerCtx IDTag m) => AST.Traversal StageTypechecker (StageTypecheckerPass m) InDecl OutDecl where
  traverseM _ = tcDecl
instance (StageTypecheckerCtx IDTag m) => AST.Traversal StageTypechecker (StageTypecheckerPass m) InExpr OutExpr where
  traverseM _ = tcExpr

traverseM :: (StageTypecheckerTraversal m a) => a -> StageTypecheckerPass m a
traverseM = AST.traverseM StageTypechecker

defaultTraverseM :: (StageTypecheckerDefaultTraversal m a) => a -> StageTypecheckerPass m a
defaultTraverseM = AST.defaultTraverseM StageTypechecker


---- top-level program

--infer :: Term -> E (TVar, Subst, Constraint, Type)
--infer e = unTP (tp (init_typo, e)) (init_tvar, null_subst, true_cons)
----


tcDecl :: (StageTypecheckerCtx IDTag m) => InDecl -> StageTypecheckerPass m OutDecl
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


          let Just lastBodyID = lastOf (traverse . Label.label . to Enum.id) body

          withClonedTypo0 $ do

              debugPush $ "Function: " ++ baseName ++ labIDdisp ++ " :: (" ++ intercalate ", " argsDisp ++ ") → ???"

              a  <- insertNewMonoTypeVariable (Enum.id lab)     -- type of the method
              bs <- forM argumentIDs insertNewMonoTypeVariable  -- types of the arguments
              
              travRes <- defaultTraverseM ldecl
              
              mResultType <- typeMap . at lastBodyID & use

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





tcExpr :: (StageTypecheckerCtx IDTag m) => LExpr IDTag () -> StageTypecheckerPass m (LExpr IDTag ())
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
  case dst of
      Pat.Var { Pat._vname = dst_vname } ->
        do  
          --tp (env, Let x e e') = do a <- tp (env, e)
          --                          b <- gen env a
          --                          tp ((insert env (x, b)), e')
          t_id <- getTargetIDString labt
          s_id <- getTargetIDString labs
          debugPush ("Assignment  " ++ unpack (humanName dst_vname) ++ t_id ++ " ⬸ " ++ s_id) 

          srcId <- getTargetID labs
          dstId <- getTargetID labt
          -- typecheck src
          res <- defaultTraverseM ass
          srcType <- getTypeById srcId

          setTypeById dstId srcType
          return res
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
    toType (Expr.AppArg _ (Label lab2 _)) = do
      getTypeById =<< getTargetID lab2

    tp result arg = do
      argType <- arg
      a <- newtvar
      add_constraint (C [result `Subsume` (argType `Fun` TV a)])
      normalize (TV a)

--expr _ = error "No idea how to infer type at the moment."
expr _ = error "inny error"







--tv_typo :: Typo -> [TVar]
--tv_typo = foldl f []
--  where
--    f z (v,ts) = z ++ tv ts








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

