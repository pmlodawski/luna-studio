{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Empire.Pass.PatternTransformation where

import           Luna.Pass        (Pass)
import qualified Luna.Pass        as Pass

import Empire.Prelude hiding (Type, String, s, new, cons)
import qualified Prologue as P hiding (List)
-- import qualified OCI.IR.Repr.Vis as Vis
-- import OCI.IR.Combinators
import qualified Data.Graph.Data.Graph.Class as Graph
import qualified Luna.Pass.Attr         as Attr
import Luna.IR (Draft, Type, Users, Terms, Model, Name, Source, Target, list, cons, tuple)
-- import Data.Graph.Component.Node.Layer.Model (inputs)
-- import Luna.Pass.Data.ExprRoots
-- import OCI.Pass.Manager
import           Luna.Pass.Data.Stage (Stage)
import qualified OCI.Pass.State.Cache as Pass
import qualified OCI.Pass.Definition.Declaration as Pass
import           Empire.Data.Layers      (TypeLayer, attachEmpireLayers)
import           Data.Text.Position      (Delta)
import           Data.Text.Span          (SpacedSpan(..), leftSpacedSpan)
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan, realSpan)
import           Data.Graph.Component.Node.Layer.PortMarker ()
import Data.Graph.Component.Node.Layer.NodeMeta   (Meta)
import Data.Graph.Component.Node.Layer.PortMarker (PortMarker)
import Data.Graph.Component.Node.Layer.SpanLength (SpanLength)
import Data.Graph.Component.Node.Layer.SpanOffset (SpanOffset)
-- import Data.TypeDesc

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import           Data.Map   (Map)


newtype ExprRoots = ExprRoots [Expr Draft]
makeWrapped ''ExprRoots
type instance Attr.Type ExprRoots = Attr.Atomic
instance Default ExprRoots where
    def = ExprRoots []

-- data EmpireStage

-- type instance Graph.Components      EmpireStage          = '[AnyExpr, AnyExprLink]
-- type instance Graph.ComponentLayers EmpireStage AnyExprLink = '[SpanOffset, Source, Target]
-- type instance Graph.ComponentLayers EmpireStage AnyExpr
--     = '[Model, Type, Users, SpanLength, CodeSpan, Meta, Marker]

data PatternTransformation
type instance Pass.Spec PatternTransformation t = PatternTransformationSpec t
type family PatternTransformationSpec t where
    PatternTransformationSpec (Pass.In  Pass.Attrs)  = '[ExprRoots]
    PatternTransformationSpec (Pass.Out Pass.Attrs)  = '[]
    PatternTransformationSpec (Pass.In  AnyExpr)     = '[Model, Type, Users, SpanLength, Meta, PortMarker, CodeSpan]
    PatternTransformationSpec (Pass.Out AnyExpr)     = '[Model, Type, Users, SpanLength, Meta, PortMarker]
    PatternTransformationSpec (Pass.In  AnyExprLink) = '[SpanOffset, Source, Target]
    PatternTransformationSpec (Pass.Out AnyExprLink) = '[SpanOffset, Source, Target]
    PatternTransformationSpec t                      = Pass.BasicPassSpec t

-- Pass.cache_phase1 ''PatternTransformation
-- Pass.cache_phase2 ''PatternTransformation

runPatternTransformation :: _ => Pass Stage PatternTransformation ()
runPatternTransformation = do
    roots <- unwrap <$> getAttr @ExprRoots
    mapM_ transformPatterns roots

data ParseError = ParseError P.String
    deriving (Show)

instance Exception ParseError where
    displayException (ParseError s) = "Parse error: " <> s

dumpConsApplication :: _ => Expr Draft -> SubPass Stage PatternTransformation (Name, [Expr Draft], [Link (Expr Draft) (Expr Draft)])
dumpConsApplication expr = matchExpr expr $ \case
    Grouped g -> dumpConsApplication . coerce =<< source g
    Cons n _  -> return (n, [], [])
    App f a   -> do
        (n, args, links) <- dumpConsApplication . coerce =<< source f
        arg <- source a
        return (n, coerce arg : args, (coerce a):links)
    _         -> throwM $ ParseError "Invalid pattern match in code"

flattenPattern :: Expr Draft -> SubPass Stage PatternTransformation (Expr Draft)
flattenPattern expr = matchExpr expr $ \case
    Grouped g -> do
        a <- flattenPattern . coerce =<< source g
        putLayer @SpanLength a =<< getLayer @SpanLength expr
        return a
    Var{}     -> return expr
    Cons{}    -> return expr
    List links   -> do
        as <- ptrListToList links
        as' <- mapM (flattenPattern . coerce <=< source) as
        l   <- generalize <$> list as'
        putLayer @SpanLength l =<< getLayer @SpanLength expr
        childLinks <- inputs l
        forM (zip as childLinks) $ \(orig, new) -> putLayer @SpanOffset new =<< getLayer @SpanOffset orig
        return l
    Tuple links   -> do
        as <- ptrListToList links
        as' <- mapM (flattenPattern . coerce <=< source) as
        t   <- generalize <$> tuple as'
        putLayer @SpanLength t =<< getLayer @SpanLength expr
        childLinks <- inputs t
        forM (zip as childLinks) $ \(orig, new) -> putLayer @SpanOffset new =<< getLayer @SpanOffset orig
        return t
    App{}     -> do
        (name, children, links) <- dumpConsApplication expr
        flatChildren            <- mapM flattenPattern $ reverse children
        res                     <- generalize <$> cons name flatChildren
        childLinks              <- inputs res
        forM (zip (reverse links) childLinks) $ \(orig, new) -> putLayer @SpanOffset new =<< getLayer @SpanOffset orig
        LeftSpacedSpan (SpacedSpan _off len) <- fmap (view CodeSpan.realSpan) $ getLayer @CodeSpan expr
        putLayer @SpanLength res len
        return res
    _         -> return expr

transformPatterns :: _ => Expr Draft -> SubPass Stage PatternTransformation ()
transformPatterns expr = matchExpr expr $ \case
    Lam i o   -> do
        inp <- coerce <$> source i
        res <- flattenPattern inp
        replace res inp
        transformPatterns . coerce =<< source o
    Unify l r -> do
        left <- coerce <$> source l
        res  <- flattenPattern left
        replace res left
        transformPatterns . coerce =<< source r
    ASGFunction n links b -> do
        as <- ptrListToList links
        forM_ as $ \a' -> do
            a   <- coerce <$> source a'
            res <- flattenPattern a
            replace res a
        transformPatterns . coerce =<< source b
    _ -> mapM_ (transformPatterns . coerce <=< source) =<< inputs expr
