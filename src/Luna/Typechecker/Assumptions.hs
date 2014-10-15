module Luna.Typechecker.Assumptions (
    Assumptions,
    mkAssumptions, searchAssumptions, expandAssumptions
  ) where

import Luna.Typechecker.IDs
import Luna.Typechecker.Substitution
import Luna.Typechecker.Type.Scheme

import qualified Data.Map.Strict as Map


--data Assump = VarID :>: Scheme

newtype Assumptions = Assumptions (Map.Map VarID Scheme)

instance Types Assumptions where
  apply s (Assumptions m) = Assumptions $ Map.map (apply s) m
  ftv (Assumptions m) = ftv (Map.elems m)

instance Show Assumptions where
  show (Assumptions m) = "Assumptions:" ++ show (Map.toList m)

mkAssumptions :: Assumptions
mkAssumptions = Assumptions Map.empty


searchAssumptions :: Assumptions -> VarID -> Maybe Scheme
searchAssumptions (Assumptions mvs) varid = Map.lookup varid mvs

expandAssumptions :: Assumptions -> VarID -> Scheme -> Assumptions
expandAssumptions (Assumptions m) v s = Assumptions $ Map.insert v s m