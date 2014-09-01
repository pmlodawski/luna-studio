module Luna.Typechecker.Internal.BindingGroups (BindGroup, tiBindGroup, tiSeq, Expr(..)) where

import           Luna.Typechecker.Internal.AST.Kind         (Kind(..))
import           Luna.Typechecker.Internal.AST.Lit          (Lit(..),tiLit)
import           Luna.Typechecker.Internal.AST.Pat          (Pat, tiPats)
import           Luna.Typechecker.Internal.AST.Scheme       (Scheme, quantify, toScheme)
import           Luna.Typechecker.Internal.AST.Type         (Type(..), fn)

import           Luna.Typechecker.Internal.Assumptions      (Assump(..),find)
import           Luna.Typechecker.Internal.Substitutions    (Types(..))
import           Luna.Typechecker.Internal.TIMonad          (TI, freshInst, getSubst, newTVar, unify)
import           Luna.Typechecker.Internal.Typeclasses      (ClassEnv, Pred(..), Qual(..), entail)
import           Luna.Typechecker.Internal.TypeInference    (Infer, split)

import           Luna.Typechecker.Internal.AST.TID          (TID)


import           Control.Monad                              (zipWithM)

import           Data.List                                  ((\\), intersect, union)



data Expr = Var TID
          | Lit Lit
          | Const Assump
          | Ap Expr Expr
          | Let BindGroup Expr
          deriving (Show)


tiExpr :: Infer Expr Type
tiExpr _  as (Var i) = do sc <- find i as
                          (ps :=> t) <- freshInst sc
                          return (ps, t)
tiExpr _  _  (Const (_ :>: sc)) = do (ps :=> t) <- freshInst sc
                                     return (ps, t)
tiExpr _  _  (Lit l) = do (ps, t) <- tiLit l
                          return (ps, t)
tiExpr ce as (Ap e f) = do (ps, te) <- tiExpr ce as e
                           (qs, tf) <- tiExpr ce as f
                           t <- newTVar Star
                           unify (tf `fn` t) te
                           return (ps ++ qs, t)
tiExpr ce as (Let bg e) = do (ps, as') <- tiBindGroup ce as bg
                             (qs, t) <- tiExpr ce (as' ++ as) e
                             return (ps ++ qs, t)



type Alt = ([Pat], Expr)


tiAlt :: Infer Alt Type
tiAlt ce as (pats, e) = do (ps, as', ts) <- tiPats pats
                           (qs, t)       <- tiExpr ce (as' ++ as) e
                           return (ps ++ qs, foldr fn t ts)

tiAlts :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
                         mapM_ (unify t . snd) psts
                         return (concatMap fst psts)



type Expl = (TID, Scheme, [Alt])

type Impl = (TID, [Alt])


-- TODO [kgdk] 20 sie 2014: 
tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (_, sc, alts) = do (qs :=> t) <- freshInst sc
                                ps         <- tiAlts ce as alts t
                                s          <- getSubst
                                let qs' = apply s qs
                                    t'  = apply s t
                                    fs  = tv (apply s as)
                                    gs  = tv t' \\ fs
                                    sc' = quantify gs (qs' :=> t')
                                    ps' = filter (not . entail ce qs') (apply s ps)
                                (ds, rs) <- split ce fs gs ps'
                                if sc /= sc' then
                                    fail "signature too general "
                                  else if not (null rs) then
                                    fail "context too weak"
                                  else
                                    return ds


-- TODO [kgdk] 20 sie 2014: 
restricted :: [Impl] -> Bool
restricted = any simple
  where simple (_, alts) = any (null . fst) alts



-- TODO [kgdk] 20 sie 2014: 
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
                      if restricted bs then
                          let gs'  = gs \\ tv rs
                              scs' = map (quantify gs' . ([] :=>)) ts'
                           in return (ds ++ rs, zipWith (:>:) is scs')
                        else
                          let scs' = map (quantify gs . (rs :=>)) ts'
                           in return (ds, zipWith (:>:) is scs')




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


