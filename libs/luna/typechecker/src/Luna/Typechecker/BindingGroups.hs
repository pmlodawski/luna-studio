module Luna.Typechecker.BindingGroups (
    BindGroup, Expl, Impl, Alt, Expr(..),
    tiSeq, tiBindGroup, tiExpl, tiExpr
  ) where


import Luna.Typechecker.Assumptions      (Assump(..),find)
import Luna.Typechecker.Substitutions    (Types(..))
import Luna.Typechecker.TIMonad          (TI,freshInst,getSubst,newTVar,unify)
import Luna.Typechecker.Typeclasses      (ClassEnv,Pred(..),Qual(..),entail)
import Luna.Typechecker.TypeInference    (Infer,split)

import Luna.Typechecker.AST.Kind         (Kind(..))
import Luna.Typechecker.AST.Lit          (Lit(..),tiLit)
import Luna.Typechecker.AST.Pat          (Pat,tiPats)
import Luna.Typechecker.AST.Scheme       (Scheme,quantify,toScheme)
import Luna.Typechecker.AST.TID          (TID(..))
import Luna.Typechecker.AST.Type         (Type(..),fn)

import Luna.Typechecker.Internal.Logger

import Control.Monad                     (filterM,unless,when,zipWithM)

import Data.List                         ((\\),intersect,union)

import Text.Printf


data Expr = Var TID
          | Lit Lit
          | EConst Assump
          | Ap Expr Expr
          | Let BindGroup Expr

instance Show Expr where
  show (Var tid)          = printf "evar %s" (show tid)
  show (Lit lit)          = printf "elit %s" (show lit)
  show (EConst (t:>:sch)) = printf "econst (%s :: %s)" (show t) (show sch)
  show (Ap e1 e2)         = printf "eap %s %s" (show e1) (show e2)
  show (Let bnd e)        = printf "elet %s = %s" (show bnd) (show e)


tiExpr :: Infer Expr Type
tiExpr _  as (Var i)             = do sc <- find i as
                                      (ps :=> t) <- freshInst sc
                                      return (ps, t)
tiExpr _  _  (EConst (_ :>: sc)) = do (ps :=> t) <- freshInst sc
                                      return (ps, t)
tiExpr _  _  (Lit l)             = do (ps, t) <- tiLit l
                                      return (ps, t)
tiExpr ce as (Ap e f)            = do (ps, te) <- tiExpr ce as e
                                      (qs, tf) <- tiExpr ce as f
                                      t <- newTVar Star
                                      unify (tf `fn` t) te
                                      return (ps ++ qs, t)
tiExpr ce as (Let bg e)          = do (ps, as') <- tiBindGroup ce as bg
                                      (qs, t) <- tiExpr ce (as' ++ as) e
                                      return (ps ++ qs, t)


type Alt = ([Pat], Expr)


tiAlt :: Infer Alt Type
tiAlt ce as (pats, e) = do (ps, as', ts) <- tiPats pats
                           (qs, t)       <- tiExpr ce (as' ++ as) e
                           return (ps ++ qs, foldr fn t ts)

tiAlts :: ClassEnv -> [Assump] -> [Alt] -> Type -> TCLoggerT TI [Pred]
tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
                         mapM_ (unify t . snd) psts
                         return (concatMap fst psts)


type Expl = (TID, Scheme, [Alt])

type Impl = (TID, [Alt])


tiExpl :: ClassEnv -> [Assump] -> Expl -> TCLoggerT TI [Pred]
tiExpl ce as (_, sc, alts) = do (qs :=> t) <- freshInst sc
                                ps         <- tiAlts ce as alts t
                                s          <- getSubst
                                let qs' = apply s qs
                                    t'  = apply s t
                                    fs  = tv (apply s as)
                                    gs  = tv t' \\ fs
                                sc' <- quantify gs (qs' :=> t')
                                ps' <- filterM (\p -> do ttt <- entail ce qs p
                                                         return (not ttt))
                                               (apply s ps)
                                (ds, rs) <- split ce fs gs ps'
                                when   (sc /= sc') $ throwError "signature too general"
                                unless (null rs)   $ throwError "context too weak"
                                return ds

restricted :: [Impl] -> Bool
restricted = any simple
  where simple (_, alts) = any (null . fst) alts

tiImpls :: Infer [Impl] [Assump]
tiImpls ce as bs = do ts <- mapM (\_ -> newTVar Star) bs
                      let is    = map fst bs
                          scs   = map toScheme ts
                          as'   = zipWith (:>:) is scs ++ as
                          altss = map snd bs
                      pss <- zipWithM (tiAlts ce as') altss ts
                      s   <- getSubst
                      let ps' = apply s (concat pss)
                          ts' = apply s ts
                          fs  = tv (apply s as)
                          vss = map tv ts'
                          gs  = foldr1 union vss \\ fs
                      (ds,rs) <- split ce fs (foldr1 intersect vss) ps'
                      if restricted bs
                        then do let gs' = gs \\ tv rs
                                scs' <- mapM (quantify gs' . ([] :=>)) ts'
                                return (ds ++ rs, zipWith (:>:) is scs')
                        else do scs' <- mapM (quantify gs . (rs :=>)) ts'
                                return (ds, zipWith (:>:) is scs')


type BindGroup = ([Expl], [[Impl]])


tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (es, iss) = do let as' = [v :>: sc | (v, sc, _) <- es]
                                 (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
                                 qss        <- mapM (tiExpl ce (as'' ++ as' ++ as)) es
                                 return (ps ++ concat qss, as'' ++ as')

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq _  _  _  [] = return ([], [])
tiSeq ti ce as (bs : bss) = do (ps, as')  <- ti ce as bs
                               (qs, as'') <- tiSeq ti ce (as' ++ as) bss
                               return (ps ++ qs, as'' ++ as')
