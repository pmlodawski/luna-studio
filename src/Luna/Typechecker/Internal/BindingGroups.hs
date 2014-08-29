module Luna.Typechecker.Internal.BindingGroups (BindGroup, tiBindGroup, tiSeq) where

import qualified Luna.Typechecker.Internal.AST.Alternatives as Alt
import qualified Luna.Typechecker.Internal.AST.Kind         as Knd
import qualified Luna.Typechecker.Internal.AST.Scheme       as Sch

import qualified Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl
import qualified Luna.Typechecker.Internal.TypeInference    as Inf

import           Luna.Typechecker.Internal.AST.TID          (TID)


import           Control.Monad                              (zipWithM)

import           Data.List                                  ((\\), intersect, union)



type Expl = (TID, Sch.Scheme, [Alt.Alt])

type Impl = (TID, [Alt.Alt])


-- TODO [kgdk] 20 sie 2014: 
tiExpl :: Tcl.ClassEnv -> [Ass.Assump] -> Expl -> TIM.TI [Tcl.Pred]
tiExpl ce as (_, sc, alts) = do (qs Tcl.:=> t) <- TIM.freshInst sc
                                ps         <- Alt.tiAlts ce as alts t
                                s          <- TIM.getSubst
                                let qs' = Sub.apply s qs
                                    t'  = Sub.apply s t
                                    fs  = Sub.tv (Sub.apply s as)
                                    gs  = Sub.tv t' \\ fs
                                    sc' = Sch.quantify gs (qs' Tcl.:=> t')
                                    ps' = filter (not . Tcl.entail ce qs') (Sub.apply s ps)
                                (ds, rs) <- Inf.split ce fs gs ps'
                                if Sub.apply s sc /= sc'
                                  then fail $ "signature too general: " ++ show sc ++ " != " ++ show sc' ++ " // " ++ show s -- yeah, we can use (==) thanks to 'Scheme' declaration and building
                                  else if not (null rs) then fail "context too weak"
                                                        else return ds


-- TODO [kgdk] 20 sie 2014: 
restricted :: [Impl] -> Bool
restricted = any simple
  where simple (_, alts) = any (null . fst) alts



-- TODO [kgdk] 20 sie 2014: 
tiImpls :: Inf.Infer [Impl] [Ass.Assump]
tiImpls ce as bs = do ts <- mapM (\_ -> TIM.newTVar Knd.Star) bs
                      let is    = map fst bs
                          scs   = map Sch.toScheme ts
                          as'   = zipWith (Ass.:>:) is scs ++ as
                          altss = map snd bs
                      pss <- zipWithM (Alt.tiAlts ce as') altss ts
                      s   <- TIM.getSubst
                      let ps' = Sub.apply s (concat pss)
                          ts' = Sub.apply s ts
                          fs  = Sub.tv (Sub.apply s as)
                          vss = map Sub.tv ts'
                          gs  = foldr1 union vss \\ fs
                      (ds,rs) <- Inf.split ce fs (foldr1 intersect vss) ps'
                      if restricted bs
                        then let gs'  = gs \\ Sub.tv rs
                                 scs' = map (Sch.quantify gs' . ([] Tcl.:=>)) ts'
                             in return (ds ++ rs, zipWith (Ass.:>:) is scs')
                        else let scs' = map (Sch.quantify gs . (rs Tcl.:=>)) ts'
                             in return (ds, zipWith (Ass.:>:) is scs')




type BindGroup = ([Expl], [[Impl]])

tiBindGroup :: Inf.Infer BindGroup [Ass.Assump]
tiBindGroup ce as (es, iss) = do let as' = [v Ass.:>: sc | (v, sc, _) <- es]
                                 (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
                                 qss        <- mapM (tiExpl ce (as'' ++ as' ++ as)) es
                                 return (ps ++ concat qss, as'' ++ as')



tiSeq :: Inf.Infer bg [Ass.Assump] -> Inf.Infer [bg] [Ass.Assump]
tiSeq _  _  _  [] = return ([], [])
tiSeq ti ce as (bs : bss) = do (ps, as')  <- ti ce as bs
                               (qs, as'') <- tiSeq ti ce (as' ++ as) bss
                               return (ps ++ qs, as'' ++ as')


