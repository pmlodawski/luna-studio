module Flowbox.Luna.Typechecker.Internal.Typeclasses (Pred(..), Qual(..), ClassEnv(..), entail, byInst) where

--import qualified Flowbox.Luna.Typechecker.Internal.AST.Alternatives as Alt
--import qualified Flowbox.Luna.Typechecker.Internal.AST.AST          as AST
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Common       as Com
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr         as Exp
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind         as Knd
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Lit          as Lit
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Module       as Mod
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat          as Pat
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Scheme       as Sch
--import qualified Flowbox.Luna.Typechecker.Internal.AST.TID          as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

--import qualified Flowbox.Luna.Typechecker.Internal.Ambiguity        as Amb
--import qualified Flowbox.Luna.Typechecker.Internal.Assumptions      as Ass
--import qualified Flowbox.Luna.Typechecker.Internal.BindingGroups    as Bnd
--import qualified Flowbox.Luna.Typechecker.Internal.ContextReduction as CxR
--import qualified Flowbox.Luna.Typechecker.Internal.HasKind          as HKd
import qualified Flowbox.Luna.Typechecker.Internal.Substitutions    as Sub
--import qualified Flowbox.Luna.Typechecker.Internal.TIMonad          as TIM
--import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl
--import qualified Flowbox.Luna.Typechecker.Internal.TypeInference    as Inf
import qualified Flowbox.Luna.Typechecker.Internal.Unification      as Unf

--import           Flowbox.Luna.Data.AST.Common                       (ID)
import           Flowbox.Luna.Typechecker.Internal.AST.TID          (TID)

import           Control.Monad                                      (msum)

import           Data.List                                          (union)
import           Data.Maybe                                         (isJust)


-- TODO [kg]: instance Functor Pred a potem zmienić lift na fmap
mguPred :: Pred -> Pred -> Maybe Sub.Subst -- why so Maybe? why not just 'm'?
mguPred = liftPred Unf.mgu

matchPred :: Pred -> Pred -> Maybe Sub.Subst -- why so Maybe? why not just 'm'?
matchPred = liftPred Unf.match

liftPred :: Monad m => (Ty.Type -> Ty.Type -> m a) -> Pred -> Pred -> m a
liftPred m (IsIn i t) (IsIn i' t') | i == i'   = m t t'
                                   | otherwise = fail "classes differ"

-- TODO [kgdk] 14 sie 2014: Z jakich monad tutaj korzystamy? Ustalić i pozbyć się `fail`



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



-- TODO [kgdk] 21 sie 2014: przesunąć wszystkie klasy na górę plików
data Pred = IsIn TID Ty.Type
          deriving (Eq, Show)


instance Sub.Types Pred where
  apply s (IsIn i t) = IsIn i (Sub.apply s t)
  tv (IsIn i t)      = Sub.tv t

-- TODO [kgdk] 14 sie 2014: napisać instancje Show dla Qual i Pred


type Class = ([TID], [Inst])
type Inst  = Qual Pred

-- TODO [kgdk] 18 sie 2014: rozważyć, które 'type' zmienić na 'newtype'/'data'


data ClassEnv = ClassEnv {
                  classes :: TID -> Maybe Class, -- TODO [kg]: IntMap, będzie trzeba TID = Int
                  defaults :: [Ty.Type]
                }


super :: ClassEnv -> TID -> [TID]
super ce i = case classes ce i of
               Just (is, its) -> is

insts :: ClassEnv -> TID -> [Inst]
insts ce i = case classes ce i of
               Just (is, its) -> its


-- TODO [kg]: how fucking stupid is this one? :<
modify :: ClassEnv -> TID -> Class -> ClassEnv
modify ce i c = ce {
                   classes = \j ->
                     if i == j
                       then Just c
                       else classes ce j
                }

-- TODO [kgdk] 18 sie 2014: zastąpić przez coś bardziej nienormalnego jak... nie wiem... Data.Map?


initialEnv :: ClassEnv
initialEnv = ClassEnv {
               classes = \i -> fail "class not defined/found",
               defaults = [Ty.tInteger, Ty.tDouble]
             }

-- TODO [kgdk] 18 sie 2014: zbadać jak zachowuje się defaulting w Lunie


type EnvTransformer = ClassEnv -> Maybe ClassEnv


-- TODO [kgdk] 18 sie 2014: zastąpić przez (>=>)
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





-- TODO [kgdk] 21 sie 2014: refactor, był problem z recursive import
-- | List predicates from superclasses: if is instance of a class, then there must be instances
-- for all superclasses.
-- If predicate 'p' then all of 'bySuper ce p' must hold as well.
-- It can contain duplicates but is always finite (since superclass hierarchy is a DAG).
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) = p : concat [bySuper ce (IsIn i' t) | i' <- super ce i]


-- | List subgoals for a predicate to match.
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t) = msum [tryInst it | it <- insts ce i] -- at most one of those from list would match (since no overlapping instances!)
  where tryInst (ps :=> h) = do u <- matchPred h p
                                Just (map (Sub.apply u) ps)


-- | Is 'p' true whenever 'ps'?
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) || case byInst ce p of
                                                           Nothing -> False
                                                           Just qs -> all (entail ce ps) qs

-- TODO [kgdk] 18 sie 2014: zmienić case na maybe

