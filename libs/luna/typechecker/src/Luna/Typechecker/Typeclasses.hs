module Luna.Typechecker.Typeclasses (
    Pred(..), Qual(..), ClassEnv(..),EnvTransformer,
    entail, byInst, addClass, addInst, (<:>), initialEnv, mguPred, matchPred, super
  ) where


import           Luna.Typechecker.Substitutions        (Types(..), Subst)
import           Luna.Typechecker.Unification          (match, mgu)

import           Luna.Typechecker.AST.TID              (TID)
import           Luna.Typechecker.AST.Type             (Type(..), tInteger, tDouble)

import           Luna.Typechecker.Internal.Logger
import           Luna.Typechecker.Internal.Typeclasses (Pred(..),Qual(..),ClassEnv(..))

import qualified Data.Map.Strict                       as M

import           Control.Monad                         ((>=>),msum,unless,when)


mguPred :: (Monad m) => Pred -> Pred -> TCLoggerT m Subst
mguPred = liftPred mgu

matchPred :: (Monad m) => Pred -> Pred -> TCLoggerT m Subst
matchPred = liftPred match

liftPred :: (Monad m) => (Type -> Type -> TCLoggerT m a) -> Pred -> Pred -> TCLoggerT m a
liftPred mf (IsIn i t) (IsIn i' t') = do unless (i == i') $ throwError "classes differ"
                                         mf t t'


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
modify ce i c = ce { classes = M.insert i c (classes ce) }

initialEnv :: ClassEnv
initialEnv = ClassEnv {
               classes = M.empty,
               defaults = [tInteger, tDouble]
             }


type EnvTransformer m = ClassEnv -> TCLoggerT m ClassEnv


infixr 5 <:>
(<:>) :: (Monad m) => EnvTransformer m -> EnvTransformer m -> EnvTransformer m
(<:>) = (>=>)


defined :: (Monad m) => TCLoggerT m a -> TCLoggerT m Bool
defined = isFine


addClass :: (Monad m) => TID -> [TID] -> EnvTransformer m
addClass i is ce | M.member i (classes ce)           = throwError "class is already defined"
                 | any (`M.notMember` classes ce) is = throwError "superclass not defined"
                 | otherwise                         = return (modify ce i (is, []))

addInst :: (Monad m) => [Pred] -> Pred -> EnvTransformer m
addInst ps p@(IsIn i _) ce 
  | M.notMember i (classes ce) = throwError "no class for instance"                         
  | otherwise                  = do its <- insts ce i
                                    overlapping <- mapM (overlap p) [q | (_ :=> q) <- its]
                                    when (or overlapping) $ throwError "overlapping instances"
                                    ca <- super ce i
                                    let c = (ca, (ps :=> p) : its)
                                    return (modify ce i c)

overlap :: (Monad m) => Pred -> Pred -> TCLoggerT m Bool
overlap p q = defined (mguPred p q)

bySuper :: (Monad m) => ClassEnv -> Pred -> TCLoggerT m [Pred]
bySuper ce p@(IsIn i t) =
  do is <- super ce i
     tails <- mapM (\i' -> bySuper ce (IsIn i' t)) is
     return (p:concat tails)

byInst :: (Monad m) => ClassEnv -> Pred -> TCLoggerT m [Pred]
byInst ce p@(IsIn i _) = do
    instances <- insts ce i
    msum (map tryInst instances)
  where tryInst :: (Monad m) => Qual Pred -> TCLoggerT m [Pred]
        tryInst (ps :=> h) = do u <- matchPred h p
                                return (map (apply u) ps)


entail :: (Monad m) => ClassEnv -> [Pred] -> Pred -> TCLoggerT m Bool
entail ce ps p = do
  sup <- mapM (bySuper ce) ps
  if any (p `elem`) sup
     then return True
     else (do ins <- byInst ce p
              ents <- mapM (entail ce ps) ins
              return (and ents)
          ) `catchError` (return . const False)
