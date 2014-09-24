module Luna.Typechecker.Typeclasses (
    Pred(..), Qual(..), ClassEnv(..),EnvTransformer,
    entail, byInst, addClass, addInst, (<:>), initialEnv, mguPred, matchPred, super
  ) where


import qualified Data.Map.Strict             as M

import Control.Monad                         ((>=>),msum,when)

import Luna.Typechecker.Substitutions        (Types(..), Subst)
import Luna.Typechecker.Unification          (match, mgu)

import Luna.Typechecker.AST.TID              (TID)
import Luna.Typechecker.AST.Type             (Type(..), tInteger, tDouble)

import Luna.Typechecker.Internal.Logger
import Luna.Typechecker.Internal.Typeclasses (Pred(..),Qual(..),ClassEnv(..))


mguPred :: (Monad m) => Pred -> Pred -> TCLoggerT m Subst
mguPred = liftPred mgu

matchPred :: (Monad m) => Pred -> Pred -> TCLoggerT m Subst
matchPred = liftPred match

liftPred :: (Monad m) => (Type -> Type -> TCLoggerT m a) -> Pred -> Pred -> TCLoggerT m a
liftPred mf (IsIn i t) (IsIn i' t') = do
  if i == i'
    then mf t t'
    else throwError "classes differ"

type Class = ([TID], [Inst])
type Inst  = Qual Pred



super :: (Monad m) => ClassEnv -> TID -> TCLoggerT m [TID]
super ce i = case M.lookup i (classes ce) of
               Just (is, _) -> return is
               Nothing -> throwError "Typeclasses.hs:super got no result"

insts :: (Monad m) => ClassEnv -> TID -> TCLoggerT m [Inst]
insts ce i = case M.lookup i (classes ce) of
               Just (_, its) -> return its
               Nothing -> throwError "Typeclasses.hs:insts got no result"

modify :: ClassEnv -> TID -> Class -> ClassEnv
modify ce i c = ce {
                   classesNames = (i,c) : classesNames ce,
                   classes = M.insert i c (classes ce)
                }

initialEnv :: ClassEnv
initialEnv = ClassEnv {
               classes = M.empty,
               classesNames = [],
               defaults = [tInteger, tDouble]
             }


type EnvTransformer m = ClassEnv -> TCLoggerT m ClassEnv


infixr 5 <:>
(<:>) :: (Monad m) => EnvTransformer m -> EnvTransformer m -> EnvTransformer m
(<:>) = (>=>)


defined :: (Monad m) => TCLoggerT m a -> TCLoggerT m Bool
defined = isFine


addClass :: (Monad m) => TID -> [TID] -> EnvTransformer m
addClass i is ce | M.member i (classes ce)                = throwError "class is already defined"
                 | any (flip M.notMember (classes ce)) is = throwError "superclass not defined"
                 | otherwise                              = return (modify ce i (is, []))

addInst :: (Monad m) => [Pred] -> Pred -> EnvTransformer m
addInst ps p@(IsIn i _) ce | M.notMember i (classes ce) = throwError "no class for instance"
                           | otherwise                  = do
  its <- insts ce i
  overlapping <- mapM (overlap p) [q | (_ :=> q) <- its]
  when (or overlapping) $ throwError "overlapping instances"
  ca <- super ce i
  let c = (ca, (ps :=> p) : its)
  return (modify ce i c)

--addInst ps p@(IsIn i _) ce | not (defined (classes ce i)) = fail "no class for instance"
--                           | any (overlap p) qs           = fail "overlapping instances"
--                           | otherwise                    = return (modify ce i c)
--  where its = insts ce i
--        qs  = [q | (_ :=> q) <- its]
--        c   = (super ce i, (ps :=> p) : its)

overlap :: (Monad m) => Pred -> Pred -> TCLoggerT m Bool
overlap p q = defined (mguPred p q)




bySuper :: (Monad m) => ClassEnv -> Pred -> TCLoggerT m [Pred]
bySuper ce p@(IsIn i t) = do
  is <- super ce i
  tails <- mapM (\i' -> bySuper ce (IsIn i' t)) is
  return (p:concat tails)
--bySuper ce p@(IsIn i t) = p : concat [bySuper ce (IsIn i' t) | i' <- super ce i]


byInst :: (Monad m) => ClassEnv -> Pred -> TCLoggerT m [Pred]
byInst ce p@(IsIn i _) = do
    instances <- insts ce i
    msum (map tryInst instances)
  where tryInst (ps :=> h) = do
          u <- matchPred h p
          return (map (apply u) ps)

--byInst ce p@(IsIn i _) = msum [tryInst it | it <- insts ce i] -- at most one of those from list would match (since no overlapping instances!)
--  where tryInst (ps :=> h) = do u <- matchPred h p
--                                Just (map (apply u) ps)


entail :: (Monad m) => ClassEnv -> [Pred] -> Pred -> TCLoggerT m Bool
entail ce ps p = do
  sup <- mapM (bySuper ce) ps
  if any (p `elem`) sup
    then return True
    else (do ins <- byInst ce p
             ents <- mapM  (entail ce ps) ins
             return (and ents)
         ) `catchError` (return . const False)
  --if any (p `elem`) sup
  --  then return True
  --  else if isFine ins
  --         then do ents <- mapM (entail ce ps) qs
  --         else return False
--entail ce ps p = any (p `elem`) (map (bySuper ce) ps) || case byInst ce p of
--                                                           Nothing -> False
--                                                           Just qs -> all (entail ce ps) qs


