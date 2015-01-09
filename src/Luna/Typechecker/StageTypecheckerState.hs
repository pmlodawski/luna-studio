{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Luna.Typechecker.StageTypecheckerState (
    StageTypechecker(..),
    StageTypecheckerState(..),
    str, typo, nextTVar, subst, constr, sa,
    StageTypecheckerPass, StageTypecheckerCtx, StageTypecheckerTraversal, StageTypecheckerDefaultTraversal
  ) where



import Control.Lens
import Text.PrettyPrint

import            Luna.Pass                     (PassMonad, PassCtx, Pass(Pass))
import Luna.Data.StructInfo (StructInfo)

import Luna.Typechecker.Data (Constraint, Subst, TVar, Typo)
import Luna.Typechecker.Debug.PrettyData
import            HumanName                     (HumanName(humanName))
import            Luna.ASTNew.Enum              (Enumerated)
import qualified  Luna.ASTNew.Pat               as Pat
import qualified  Luna.ASTNew.Traversals        as AST
import Luna.Typechecker.Debug.PrettyData



data StageTypechecker = StageTypechecker

data StageTypecheckerState
   = StageTypecheckerState  { _str      :: [String]
                            , _typo     :: [Typo]
                            , _nextTVar :: TVar
                            , _subst    :: Subst
                            , _constr   :: Constraint
                            , _sa       :: StructInfo
                            }

makeLenses ''StageTypecheckerState

type StageTypecheckerPass             m       = PassMonad StageTypecheckerState m
type StageTypecheckerCtx              lab m a = (HumanName (Pat.Pat lab), Enumerated lab, StageTypecheckerTraversal m a)
type StageTypecheckerTraversal        m   a   = (PassCtx m, AST.Traversal        StageTypechecker (StageTypecheckerPass m) a a)
type StageTypecheckerDefaultTraversal m   a   = (PassCtx m, AST.DefaultTraversal StageTypechecker (StageTypecheckerPass m) a a)



instance Show StageTypecheckerState where show = render . prettyState



prettyState :: StageTypecheckerState -> Doc
prettyState StageTypecheckerState{..} = str_field
                                    $+$ constr_field
                                    $+$ typo_field
                                    $+$ subst_field
                                    $+$ nextTVar_field
  where
    str_field      = text "Debug       :" <+> prettyNullable (map text _str)
    constr_field   = text "Constraints :" <+> prettyConstr   _constr
    nextTVar_field = text "TVars used  :" <+> int         _nextTVar
    typo_field     = text "Type env    :" <+> prettyNullable (map (parens . prettyTypo) _typo)
    subst_field    = text "Substs      :" <+> prettySubst    _subst
