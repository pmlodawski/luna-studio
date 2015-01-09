{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Luna.Typechecker.StageTypecheckerState (
    StageTypecheckerState(..),
    StageTypechecker(..),
    StageTypecheckerPass,
    StageTypecheckerCtx,
    StageTypecheckerTraversal,
    StageTypecheckerDefaultTraversal,
    str, typo, nextTVar, subst, constr, sa,
    prettyState,
    report_error
  ) where



import            Control.Lens                      (makeLenses)
import            Control.Monad.IO.Class            (MonadIO, liftIO)
import            Text.PrettyPrint

import            Luna.Pass                         (PassMonad, PassCtx, Pass(Pass))
import            Luna.Data.StructInfo              (StructInfo)
import            Luna.ASTNew.Enum                  (Enumerated)
import qualified  Luna.ASTNew.Traversals            as AST
import qualified  Luna.ASTNew.Pat                   as Pat

import            Luna.Typechecker.Data             (Constraint, Subst, TVar, Typo)
import            Luna.Typechecker.Debug.HumanName  (HumanName(humanName))
import            Luna.Typechecker.Debug.PrettyData (prettyConstr, prettyNullable, prettySubst, prettyTypo)



data StageTypecheckerState
   = StageTypecheckerState  { _str      :: [String]
                            , _typo     :: [Typo]
                            , _nextTVar :: TVar
                            , _subst    :: Subst
                            , _constr   :: Constraint
                            , _sa       :: StructInfo
                            }
makeLenses ''StageTypecheckerState


data StageTypechecker = StageTypechecker

type StageTypecheckerPass             m       = PassMonad StageTypecheckerState m
type StageTypecheckerCtx              lab m a = (HumanName (Pat.Pat lab), Enumerated lab, StageTypecheckerTraversal m a)
type StageTypecheckerTraversal        m   a   = (PassCtx m, AST.Traversal        StageTypechecker (StageTypecheckerPass m) a a)
type StageTypecheckerDefaultTraversal m   a   = (PassCtx m, AST.DefaultTraversal StageTypechecker (StageTypecheckerPass m) a a)



report_error :: (Monad m) =>  String -> a ->  StageTypecheckerPass m a
report_error msg x = fail $ "LUNA ERROR: " ++ msg




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
