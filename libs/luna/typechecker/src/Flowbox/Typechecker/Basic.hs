{-# LANGUAGE RecordWildCards #-}

module           Flowbox.Typechecker.Basic where


import qualified Flowbox.Luna.Data.AST.Module as Module
import qualified Flowbox.Luna.Data.AST.Common as Common

import           Flowbox.Luna.Data.AST.Common (ID(..))
import           Flowbox.Luna.Data.AST.Expr   (Expr(..))
import           Flowbox.Luna.Data.AST.Lit    (Lit(..))
import           Flowbox.Luna.Data.AST.Type   (Type(..))

import qualified Data.Set                     as S
import qualified Data.Map                     as M



class Types a where
    ftv :: a -> S.Set ID
    apply :: Subst -> a -> a

instance Types a => Types [a] where
    ftv l = foldr S.union S.empty (map ftv l)
    apply s = map (apply s)

instance Types Expr where
    ftv _ = S.empty
    apply _ x= x

instance Types Lit where
    ftv _ = S.empty
    apply _ x = x




type Subst = M.Map ID Type

nullSubst :: Subst
nullSubst = M.empty


data Scheme = Scheme [ID] Type



typecheck :: Module.Module -> Maybe Module.Module
typecheck = undefined