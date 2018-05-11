{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
module Empire.Prelude (module X, nameToString, pathNameToString, stringToName,
                      (<?!>), putLayer, attachLayer, AnyExpr, AnyExprLink,
                      makeWrapped, Expr, SubPass, pattern MarkedExprMap,
                      pattern ASGFunction, pattern Lam, pattern Unify,
                      pattern App, pattern Cons, pattern Grouped, pattern Var,
                      pattern List, pattern Tuple, getAttr, putAttr,
                      source, target, getLayer, type Link, matchExpr,
                      ptrListToList, generalize, replace, makePrisms,
                      type SomeExpr, use, preuse, (?=), (.=), (%=), to, _Just,
                      (?~), pattern Seq, pattern Marked, pattern Blank,
                      pattern Acc, pattern Documented, narrowTerm,
                      pattern Unit, replaceSource, modifyLayer_,
                      type LeftSpacedSpan, pattern LeftSpacedSpan,
                      ociSetToList, pattern IRString, pattern IRNumber,
                      nameToText, type MarkedExprMap, deleteSubtree, substitute,
                      unsafeRelayout, irDelete, link, modifyExprTerm,
                      type ASGFunction, pattern LeftSection, zoom,
                      pattern RightSection, pattern Missing, pattern Marker,
                      type IRSuccs, deepDelete, deepDeleteWithWhitelist,
                      pattern ClsASG, type ClsASG, pattern Metadata) where

import qualified Data.Convert              as Convert
import qualified Data.Typeable as Typeable
import qualified Data.PtrList.Mutable as PtrList
import qualified Data.PtrSet.Mutable as PtrSet
import qualified Data.Graph.Component.Container as PtrSet
import qualified Data.Text.Span as Span
import qualified Data.Graph.Component as Component
import qualified Data.Graph.Component.Layer as Layer
import qualified Data.Graph.Component.Layout as Layout
import qualified OCI.Pass.Registry as Registry
import qualified Luna.IR as IR
import OCI.IR.Link.Class (type (*-*), Links)
import OCI.IR.Term.Class (Term, Terms)
import qualified Luna.IR.Term.Core as Ast
import qualified Luna.IR.Term.Literal as Ast
import qualified Luna.IR.Term.Ast.Class as Ast
import qualified OCI.IR.Link.Construction as Construction
import Luna.Syntax.Text.Parser.State.Marker (TermMap(..))
import Luna.Pass (Pass)
import qualified Luna.Pass.Attr as Attr
import Control.Lens ((?=), (.=), (%=), to, makeWrapped, makePrisms, use, preuse,
                     _Just, (?~), zoom)
import Prologue as X hiding (TypeRep, head, tail, init, last, p, r, s, (|>), return, liftIO, fromMaybe, fromJust, when, mapM, mapM_)
import Control.Monad       as X (return, when, mapM, mapM_, forM)
import Control.Monad.Trans as X (liftIO)
import Data.List           as X (head, tail, init, last, sort)
import Data.Maybe          as X (fromJust, fromMaybe)

infixr 0 <?!>
(<?!>) :: (Exception e, MonadThrow m) => m (Maybe a) -> e -> m a
m <?!> e = m >>= maybe (throwM e) pure


-- nameToString :: IR.Name -> String
nameToString = error "nameToString"

nameToText :: IR.Name -> Text
nameToText = convert . convertTo @String

-- pathNameToString :: IR.QualName -> String
pathNameToString = error "pathNameToString"

-- stringToName :: String -> IR.Name
stringToName = error "stringToName"

putLayer :: forall layer t layout m. Layer.Writer t layer m => t layout -> Layer.Data layer layout -> m ()
putLayer = Layer.write @layer

getLayer :: forall layer t layout m. Layer.Reader t layer m => t layout -> m (Layer.Data layer layout)
getLayer = Layer.read @layer

modifyLayer_ :: forall layer t layout m. (Layer.Reader t layer m, Layer.Writer t layer m) => t layout -> (Layer.Data layer layout -> Layer.Data layer layout) -> m ()
modifyLayer_ comp f = getLayer @layer comp >>= putLayer @layer comp . f


getTypeDesc_ :: forall a. (KnownType a, Typeable a) => Typeable.TypeRep
getTypeDesc_ = Typeable.typeRep (Proxy @a)

attachLayer :: forall comp layer m. _ => m ()
attachLayer = Registry.registerPrimLayer @layer @comp

type AnyExpr = Terms
type AnyExprLink = Links
type Expr = Term
type SubPass = Pass
type Link a b = IR.Link (a *-* b)
type SomeExpr = IR.SomeTerm
type LeftSpacedSpan a = Span.LeftSpacedSpan
type IRSuccs = IR.Users

pattern LeftSpacedSpan a <- Span.LeftSpacedSpan a where
    LeftSpacedSpan a = Span.LeftSpacedSpan a

type MarkedExprMap = TermMap
pattern MarkedExprMap m <- TermMap m where
    MarkedExprMap m = TermMap m

type ASGFunction = IR.Function
type ClsASG = IR.Record

pattern ASGFunction n as b <- IR.UniTermFunction (Ast.Function n as b)
pattern LeftSection f a <- IR.UniTermSectionLeft (Ast.SectionLeft f a)
pattern RightSection f a <- IR.UniTermSectionRight (Ast.SectionRight f a)
pattern Unit n as b <- IR.UniTermUnit (Ast.Unit n as b)
pattern Unify l r <- IR.UniTermUnify (Ast.Unify l r)
pattern Lam i o <- IR.UniTermLam (Ast.Lam i o)
pattern Missing <- IR.UniTermMissing (Ast.Missing)
pattern App f a <- IR.UniTermApp (Ast.App f a)
pattern Cons n a <- IR.UniTermCons (Ast.Cons n a)
pattern Grouped g <- IR.UniTermGrouped (Ast.Grouped g)
pattern Var n <- IR.UniTermVar (Ast.Var n)
pattern Marked m n <- IR.UniTermMarked (Ast.Marked m n)
pattern Marker  l <- IR.UniTermMarker (Ast.Marker l)
pattern List  l <- IR.UniTermList (Ast.List l)
pattern Tuple t <- IR.UniTermTuple (Ast.Tuple t)
pattern Seq l r <- IR.UniTermSeq (Ast.Seq l r)
pattern Blank <- IR.UniTermBlank (Ast.Blank)
pattern Acc n e <- IR.UniTermAcc (Ast.Acc n e)
pattern Documented doc e <- IR.UniTermDocumented (Ast.Documented doc e)
pattern IRString s <- IR.UniTermString (Ast.String s)
pattern IRNumber a b c <- IR.UniTermNumber (Ast.Number a b c)
pattern ClsASG a b c d e <- IR.UniTermRecord (Ast.Record a b c d e)
pattern Metadata a <- IR.UniTermMetadata (Ast.Metadata a)

getAttr :: forall attr m. (Monad m, Attr.Getter attr m) => m attr
putAttr :: forall attr m. (Monad m, Attr.Setter attr m) => attr -> m ()
getAttr = Attr.get @attr
putAttr = Attr.put

source :: (Layer.Reader IR.Link IR.Source m, Coercible (Expr t) (Expr (Layout.Get IR.Source layout)))
       => IR.Link layout -> m (Expr t)
source = \a -> IR.source a >>= \b -> return (coerce b)

target :: (Layer.Reader IR.Link IR.Target m, Coercible (Expr t) (Expr (Layout.Get IR.Target layout)))
       => IR.Link layout -> m (Expr t)
target = \a -> IR.target a >>= \b -> return (coerce b)


matchExpr :: forall t layout m. Layer.Reader t IR.Model m => t layout -> (IR.UniTerm layout -> _) -> m _
matchExpr e f = Layer.read @IR.Model e >>= f

ptrListToList :: (MonadIO m, PtrList.IsPtrList t, PtrList.IsPtr a) => t a -> m [a]
ptrListToList = PtrList.toList

ociSetToList :: (MonadIO m, PtrSet.IsPtr a) => PtrSet.Set c l -> m [a]
ociSetToList = PtrSet.toList . (coerce :: PtrSet.Set c l -> PtrSet.UnmanagedPtrSet a)

generalize :: Coercible a b => a -> b
generalize = coerce

replace :: Monad m => Expr l -> Expr l' -> m ()
replace = error "replace"

irDelete = error "delete"

substitute = error "substitute"

unsafeRelayout :: Coercible a b => a -> b
unsafeRelayout = coerce

replaceSource :: Monad m => Expr l -> Link l t -> m ()
replaceSource = error "replaceSource"

narrowTerm :: forall b m a. Monad m => Expr a -> m (Maybe (Expr b))
narrowTerm e = error "narrowTerm"

deleteSubtree = error "deleteSubtree"
deepDelete = error "deepDelete"
deepDeleteWithWhitelist = error "deepDeleteWithWhitelist"


link :: Construction.Creator m => Term src -> Term tgt -> m (IR.Link (src *-* tgt))
link = Construction.new

modifyExprTerm = error "modifyExprTerm"