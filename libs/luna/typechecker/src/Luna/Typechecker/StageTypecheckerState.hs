{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Luna.Typechecker.StageTypecheckerState (
    StageTypecheckerState(..),
    debugLog, typo, nextTVar, subst, constr, sa, typeMap,
    StageTypechecker(..),
    StageTypecheckerPass, StageTypecheckerCtx, StageTypecheckerTraversal, StageTypecheckerDefaultTraversal,
    InExpr, OutExpr, InDecl, OutDecl,
    prettyState,
    report_error
  ) where



import            Luna.Data.StructInfo              (StructInfo)
import            Luna.Pass                         (PassMonad, PassCtx)
import qualified  Luna.Syntax.Decl                  as Decl
import            Luna.Syntax.Enum                  (Enumerated, IDTag)
import qualified  Luna.Syntax.Expr                  as Expr
import qualified  Luna.Syntax.Pat                   as Pat
import qualified  Luna.System.Session               as Session
import qualified  Luna.Syntax.Traversals            as AST

import            Control.Applicative
import            Control.Lens
import            Control.Monad.IO.Class            (MonadIO)
import            Control.Monad.State.Lazy          (get)
import            Data.Monoid                       (Monoid(..))
import            Text.PrettyPrint

import            Data.Default                      (Default(def))

import            Luna.Typechecker.Data             (Constraint, Subst, TVar, Typo, TypeMap, init_typo, null_subst, true_cons)
import            Luna.Typechecker.Debug.HumanName  (HumanName)
import            Luna.Typechecker.Debug.PrettyData (
                      prettyConstr, prettyNullable, prettySubst, prettyTypo, prettyTypeMap
                  )



data StageTypecheckerState
   = StageTypecheckerState  { _debugLog    :: [String]
                            , _typo        :: [Typo]
                            , _nextTVar    :: TVar
                            , _subst       :: Subst
                            , _constr      :: Constraint
                            , _sa          :: StructInfo
                            , _typeMap     :: TypeMap
                            }
makeLenses ''StageTypecheckerState

instance Default StageTypecheckerState where
  def = StageTypecheckerState { _debugLog = []
                              , _typo     = init_typo
                              , _nextTVar = 0
                              , _subst    = null_subst
                              , _constr   = true_cons
                              , _sa       = mempty
                              , _typeMap  = mempty
                              }


data StageTypechecker = StageTypechecker

type StageTypecheckerPass                 m   = PassMonad StageTypecheckerState m
type StageTypecheckerCtx              lab m   = (HumanName (Pat.Pat lab), Enumerated lab, Monad m, Applicative m, MonadIO m, Session.SessionMonad m, PassCtx m)
type StageTypecheckerTraversal            m a = (PassCtx m, AST.Traversal        StageTypechecker (StageTypecheckerPass m) a a)
type StageTypecheckerDefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal StageTypechecker (StageTypecheckerPass m) a a)

type InExpr  = (Expr.LExpr IDTag ())
type OutExpr = (Expr.LExpr IDTag ()) 

type InDecl  = (Decl.LDecl IDTag InExpr)
type OutDecl = (Decl.LDecl IDTag InExpr)



report_error :: (Monad m) => String -> a ->  StageTypecheckerPass m a
report_error msg x = do
  st <- get
  let msgRes = "LUNA TC ERROR: " ++ msg ++ "\nState:\n\n" ++ show st
  fail msgRes




instance Show StageTypecheckerState where show = render . prettyState

prettyState :: StageTypecheckerState -> Doc
prettyState StageTypecheckerState{..} = str_field
                                    $+$ constr_field
                                    $+$ typo_field
                                    $+$ subst_field
                                    $+$ nextTVar_field
                                    $+$ typeMap_field
  where
    str_field      = text "Debug       :" <+> prettyNullable (map text $ reverse _debugLog)
    constr_field   = text "Constraints :" <+> prettyConstr   _constr
    nextTVar_field = text "TVars used  :" <+> int         _nextTVar
    typo_field     = text "Type env    :" <+> prettyNullable (map (parens . prettyTypo) _typo)
    subst_field    = text "Substs      :" <+> prettySubst    _subst
    typeMap_field  = text "Type map    :" <+> prettyTypeMap _typeMap
