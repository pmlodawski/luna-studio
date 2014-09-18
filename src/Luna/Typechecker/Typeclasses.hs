module Luna.Typechecker.Typeclasses (
    Pred(..), Qual(..), ClassEnv(..),EnvTransformer,
    entail, byInst, addClass, addInst, (<:>), initialEnv, mguPred, matchPred, super
  ) where

import Luna.Typechecker.Internal.Typeclasses (Pred(..),Qual(..),ClassEnv(..))

import Luna.Typechecker.AST.Type       (Type(..), tInteger, tDouble)

import Luna.Typechecker.Substitutions  (Types(..), Subst)
import Luna.Typechecker.Unification    (match, mgu)

import Luna.Typechecker.AST.TID        (TID)

import Control.Monad                   (msum)

--import Data.List                       (intercalate,union,nubBy)
import Data.Maybe                      (isJust)
--import Data.Function                   (on)
--import Text.Printf                     (printf)
--import Control.DeepSeq


mguPred :: Pred -> Pred -> Maybe Subst
mguPred = liftPred mgu

matchPred :: Pred -> Pred -> Maybe Subst
matchPred = liftPred match

liftPred :: Monad m => (Type -> Type -> m a) -> Pred -> Pred -> m a
liftPred m (IsIn i t) (IsIn i' t') | i == i'   = m t t'
                                   | otherwise = fail "classes differ"



type Class = ([TID], [Inst])
type Inst  = Qual Pred



super :: ClassEnv -> TID -> [TID]
super ce i = case classes ce i of
               Just (is, _) -> is
               Nothing -> error "Typeclasses.hs:super got no result"

insts :: ClassEnv -> TID -> [Inst]
insts ce i = case classes ce i of
               Just (_, its) -> its
               Nothing -> error "Typeclasses.hs:insts got no result"


-- TODO [kg]: how fucking stupid is this one? :<
modify :: ClassEnv -> TID -> Class -> ClassEnv
modify ce i c = ce {
                   classesNames = (i,c) : classesNames ce,
                   classes = \j ->
                     if i == j
                       then Just c
                       else classes ce j
                }

initialEnv :: ClassEnv
initialEnv = ClassEnv {
               classes = \_ -> fail "class not defined/found",
               classesNames = [],
               defaults = [tInteger, tDouble]
             }

-- TODO [kgdk] 18 sie 2014: zbadać jak zachowuje się defaulting w Lunie


type EnvTransformer = ClassEnv -> Maybe ClassEnv


infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do ce' <- f ce
                  g ce'


defined :: Maybe a -> Bool
defined = isJust


addClass :: TID -> [TID] -> EnvTransformer
addClass i is ce | defined (classes ce i)              = fail "class is already defined"
                 | any (not . defined . classes ce) is = fail "superclass not defined"
                 | otherwise                           = return (modify ce i (is, []))

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce | not (defined (classes ce i)) = fail "no class for instance"
                           | any (overlap p) qs           = fail "overlapping instances"
                           | otherwise                    = return (modify ce i c)
  where its = insts ce i
        qs  = [q | (_ :=> q) <- its]
        c   = (super ce i, (ps :=> p) : its)

overlap :: Pred -> Pred -> Bool
overlap p q = defined (mguPred p q)





-- | List predicates from superclasses: if is instance of a class, then there must be instances
-- for all superclasses.
-- If predicate 'p' then all of 'bySuper ce p' must hold as well.
-- It can contain duplicates but is always finite (since superclass hierarchy is a DAG).
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) = p : concat [bySuper ce (IsIn i' t) | i' <- super ce i]


-- | List subgoals for a predicate to match.
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i _) = msum [tryInst it | it <- insts ce i] -- at most one of those from list would match (since no overlapping instances!)
  where tryInst (ps :=> h) = do u <- matchPred h p
                                Just (map (apply u) ps)


-- | Is 'p' true whenever 'ps'?
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) || case byInst ce p of
                                                           Nothing -> False
                                                           Just qs -> all (entail ce ps) qs
