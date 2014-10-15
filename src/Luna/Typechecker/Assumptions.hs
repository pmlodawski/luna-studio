module Luna.Typechecker.Assumptions (
    Assumptions,
    mkAssumptions, assumptionsSearch
  ) where

import Luna.Typechecker.IDs
import Luna.Typechecker.Type.Scheme

import qualified Data.Map.Strict as Map


--data Assump = VarID :>: Scheme

newtype Assumptions = Assumptions (Map.Map VarID Scheme)

instance Show Assumptions where
  show (Assumptions _) = "here insert Show Assumptions instance"

mkAssumptions :: Assumptions
mkAssumptions = Assumptions $ Map.empty


assumptionsSearch :: Assumptions -> VarID -> Maybe Scheme
assumptionsSearch (Assumptions mvs) varid = Map.lookup varid mvs
