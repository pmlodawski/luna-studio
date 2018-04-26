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
                      nameToText, type MarkedExprMap) where

import qualified Data.Convert              as Convert
import qualified Data.Typeable as Typeable
import qualified Data.PtrList.Mutable as PtrList
import qualified Data.PtrSet.Mutable as PtrSet
import qualified OCI.IR.Component.Container as PtrSet
import qualified Data.Text.Span as Span
import qualified OCI.IR.Component as Component
import qualified OCI.IR.Layer as Layer
import qualified OCI.IR.Layout as Layout
import qualified OCI.Pass.Registry as Registry
import qualified Luna.IR as IR
import Luna.IR.Component.Link.Class (type (*-*), Links)
import Luna.IR.Component.Term.Class (Term, Terms)
import qualified Luna.IR.Term.Core as Ast
import qualified Luna.IR.Term.Literal as Ast
import qualified Luna.IR.Term.Ast.Class as Ast
import Luna.Syntax.Text.Parser.State.Marker (TermMap(..))
import Luna.Pass (Pass)
import qualified Luna.Pass.Attr as Attr
import Control.Lens ((?=), (.=), (%=), to, makeWrapped, makePrisms, use, preuse,
                     _Just, (?~))
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

putLayer :: forall layer comp layout m. Layer.Writer comp layer m => Component.Component comp layout -> Layer.Data layer layout -> m ()
putLayer = Layer.write @layer

getLayer :: forall layer comp layout m. Layer.Reader comp layer m => Component.Component comp layout -> m (Layer.Data layer layout)
getLayer = Layer.read @layer

modifyLayer_ :: forall layer comp layout m. (Layer.Reader comp layer m, Layer.Writer comp layer m) => Component.Component comp layout -> (Layer.Data layer layout -> Layer.Data layer layout) -> m ()
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

pattern LeftSpacedSpan a <- Span.LeftSpacedSpan a where
    LeftSpacedSpan a = Span.LeftSpacedSpan a

type MarkedExprMap = TermMap
pattern MarkedExprMap m <- TermMap m where
    MarkedExprMap m = TermMap m

pattern ASGFunction n as b <- IR.UniTermFunction (Ast.Function n as b)
pattern Unit n as b <- IR.UniTermUnit (Ast.Unit n as b)
pattern Unify l r <- IR.UniTermUnify (Ast.Unify l r)
pattern Lam i o <- IR.UniTermLam (Ast.Lam i o)
pattern App f a <- IR.UniTermApp (Ast.App f a)
pattern Cons n a <- IR.UniTermCons (Ast.Cons n a)
pattern Grouped g <- IR.UniTermGrouped (Ast.Grouped g)
pattern Var n <- IR.UniTermVar (Ast.Var n)
pattern Marked m n <- IR.UniTermMarked (Ast.Marked m n)
pattern List  l <- IR.UniTermList (Ast.List l)
pattern Tuple t <- IR.UniTermTuple (Ast.Tuple t)
pattern Seq l r <- IR.UniTermSeq (Ast.Seq l r)
pattern Blank <- IR.UniTermBlank (Ast.Blank)
pattern Acc n e <- IR.UniTermAcc (Ast.Acc n e)
pattern Documented doc e <- IR.UniTermDocumented (Ast.Documented doc e)
pattern IRString s <- IR.UniTermString (Ast.String s)
pattern IRNumber a b c <- IR.UniTermNumber (Ast.Number a b c)

getAttr :: forall attr m. (Monad m, Attr.Getter attr m) => m attr
putAttr :: forall attr m. (Monad m, Attr.Setter attr m) => attr -> m ()
getAttr = Attr.get @attr
putAttr = Attr.put

source :: (Layer.Reader Links IR.Source m, Coercible (Expr t) (Expr (Layout.Get IR.Source layout)))
       => IR.Link layout -> m (Expr t)
source = \a -> IR.source a >>= \b -> return (coerce b)

target :: (Layer.Reader Links IR.Target m, Coercible (Expr t) (Expr (Layout.Get IR.Target layout)))
       => IR.Link layout -> m (Expr t)
target = \a -> IR.target a >>= \b -> return (coerce b)


matchExpr :: forall comp layout m. Layer.Reader comp IR.Model m => Component.Component comp layout -> (IR.UniTerm layout -> _) -> m _
matchExpr e f = Layer.read @IR.Model e >>= f

ptrListToList :: (MonadIO m, PtrList.IsPtrList t, PtrList.IsPtr a) => t a -> m [a]
ptrListToList = PtrList.toList

ociSetToList :: (MonadIO m, PtrSet.IsPtr a) => PtrSet.Set c l -> m [a]
ociSetToList = PtrSet.toList . (coerce :: PtrSet.Set c l -> PtrSet.UnmanagedPtrSet a)

generalize :: Coercible a b => a -> b
generalize = coerce

replace :: Expr l -> Expr l' -> Pass t ()
replace = error "replace"

replaceSource :: Monad m => Expr l -> Link l t -> m ()
replaceSource = error "replaceSource"

narrowTerm :: forall b m a. Monad m => Expr a -> m (Maybe (Expr b))
narrowTerm e = error "narrowTerm"