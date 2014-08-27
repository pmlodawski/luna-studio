module Luna.Typechecker.Internal.Typeclasses (
    Pred(..), Qual(..), ClassEnv(..),
    entail, byInst, addClass, addInst, (<:>)
  ) where

import qualified Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Luna.Typechecker.Internal.Unification      as Unf

import           Luna.Typechecker.Internal.AST.TID          (TID)

import           Control.Monad                                      (msum)

import           Data.List                                          (union)
import           Data.Maybe                                         (isJust)


data Pred = IsIn TID Ty.Type
          deriving (Eq, Show)


mguPred :: Pred -> Pred -> Maybe Sub.Subst
mguPred = liftPred Unf.mgu

matchPred :: Pred -> Pred -> Maybe Sub.Subst
matchPred = liftPred Unf.match

liftPred :: Monad m => (Ty.Type -> Ty.Type -> m a) -> Pred -> Pred -> m a
liftPred m (IsIn i t) (IsIn i' t') | i == i'   = m t t'
                                   | otherwise = fail "classes differ"


-- | Qualify.
-- Used for qualifying types and for class dependencies.
-- E.g.: '(Num a) => a -> Int' is represented as
--   [IsIn "Num" (TVar (Tyvar "a" Star))] :=> (TVar (Tyvar "a" Star) `fn` tInt)
-- E.g.: to state, that the instance 'Ord (a,b)' requires 'Ord a' and 'Ord b':
--    [IsIn "Ord" (TVar (Tyvar "a" Star)), IsIn "Ord" (TVar (Tyvar "b" Star))]
--      :=>
--    IsIn "Ord" (pair (TVar (Tyvar "a" Star)) (TVar (Tyvar "b" Star)))]
data Qual t = [Pred] :=> t
            deriving (Eq, Show)

instance Sub.Types t => Sub.Types (Qual t) where
  apply s (ps :=> t) = Sub.apply s ps :=> Sub.apply s t
  tv (ps :=> t)      = Sub.tv ps `union` Sub.tv t



instance Sub.Types Pred where
  apply s (IsIn i t) = IsIn i (Sub.apply s t)
  tv (IsIn _ t)      = Sub.tv t


type Class = ([TID], [Inst])
type Inst  = Qual Pred


data ClassEnv = ClassEnv {
                  classes :: TID -> Maybe Class,
                  defaults :: [Ty.Type]
                }


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
                   classes = \j ->
                     if i == j
                       then Just c
                       else classes ce j
                }

--initialEnv :: ClassEnv
--initialEnv = ClassEnv {
--               classes = \_ -> fail "class not defined/found",
--               defaults = [Ty.tInteger, Ty.tDouble]
--             }

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
                                Just (map (Sub.apply u) ps)


-- | Is 'p' true whenever 'ps'?
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) || case byInst ce p of
                                                           Nothing -> False
                                                           Just qs -> all (entail ce ps) qs
