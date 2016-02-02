{-# LANGUAGE TypeFamilies #-}

module Luna.StdLib.Int where

import           Prologue
import qualified Data.Map          as Map
import           Data.Layer.Coat   (Coated, Uncoated, CoatConstructor)
import           Data.Construction (Destructed)
import           Data.Maybe        (maybeToList)

import qualified Luna.Syntax.Builder           as Builder
import           Luna.Syntax.Builder.Class     (BuilderT)
import qualified Luna.Syntax.Builder.Star      as StarBuilder
import           Luna.Syntax.Builder.Star      (StarBuilderT, MonadStarBuilder)
import qualified Luna.Syntax.Builder.Node      as NodeBuilder
import           Luna.Syntax.Builder.Node      (NodeBuilderT)
import           Luna.Syntax.AST.Decl.Function (Function(..))
import           Luna.Syntax.Symbol.Map        (SymbolMap)
import qualified Luna.Syntax.Symbol.QualPath   as QualPath
import           Luna.Syntax.Repr.Graph        (Graph, DoubleArc, Ref(..), Node(..), Edge, TracksSuccs)
import           Luna.Syntax.AST.Term          (Draft)

type NodeType a      = (Coated a, Uncoated a ~ (Draft (Ref Edge)), TracksSuccs a, CoatConstructor (BuilderAction a) a)
type BuilderAction a = NodeBuilderT (Ref Node)
                                      (BuilderT (Graph a DoubleArc)
                                                (StarBuilderT (Maybe (Ref Node))
                                                              Identity))

type FunBuilder a    = BuilderAction a (Maybe (Ref Node), [Ref Node], Ref Node)

buildGraph :: NodeType a => BuilderAction a b -> (b, Graph a DoubleArc)
buildGraph m = runIdentity
             $ flip StarBuilder.evalT Nothing
             $ flip Builder.runT def
             $ flip NodeBuilder.evalT (Ref $ Node 0)
             $ m

makeFunction :: NodeType a => FunBuilder a -> Function (Graph a DoubleArc)
makeFunction bldr = Function self args out body where
    ((self, args, out), body) = buildGraph bldr

typed :: MonadStarBuilder (Maybe (Ref Node)) m => m a -> Ref Node -> m a
typed a t = StarBuilder.with (const $ Just t) a

makeNativeFun :: NodeType a => String -> Maybe String -> [String] -> String -> FunBuilder a
makeNativeFun name selfTypeStr argTypesStr outTypeStr = do
    selfType <- mapM Builder.cons selfTypeStr
    argTypes <- mapM Builder.cons argTypesStr
    outType  <- Builder.cons outTypeStr
    self <- mapM (typed Builder._blank) selfType
    args <- mapM (typed Builder._blank) argTypes
    let nativeArgs = Builder.arg <$> maybeToList self ++ args
    native <- Builder.native name nativeArgs `typed` outType
    return (self, args, native)

symbols :: NodeType a => SymbolMap (Graph a DoubleArc)
symbols = Map.fromList $ fmap (\(n, b) -> (QualPath.mk (n :: String), makeFunction b))
    [ ("Int.+",        makeNativeFun "(+)"  (Just "Int") ["Int"] "Int")
    , ("Int.*",        makeNativeFun "(*)"  (Just "Int") ["Int"] "Int")
    , ("Int.toString", makeNativeFun "show" (Just "Int") []      "String")
    , ("replicate"   , makeNativeFun "replicate" Nothing ["Int", "Double"] "[Double]")
    , ("Int.toDouble", makeNativeFun "fromIntegral" (Just "Int") [] "Double")
    ]
