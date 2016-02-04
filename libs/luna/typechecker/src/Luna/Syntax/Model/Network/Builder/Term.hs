{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RecursiveDo            #-}

module Luna.Syntax.Model.Network.Builder.Term where

import Prelude.Luna hiding (Num)

import           Data.Record                    (RecordOf, IsRecord, asRecord, SmartCons, Variant, MapTryingElemList_, withElement_, Props)
import qualified Data.Record                    as Record
import qualified Luna.Syntax.AST.Term           as Term
import           Luna.Syntax.AST.Term           hiding (Val, Lit, Thunk, Expr, Draft)
import Luna.Syntax.Model.Graph
import Data.Prop
import           Control.Monad.Event
import           Luna.Runtime.Model
import           Luna.Syntax.Model.Network.Builder.Layer
import qualified Luna.Syntax.Model.Graph.Builder as GraphBuilder
import qualified Luna.Syntax.Model.Network.Builder.Type as Type
import qualified Luna.Syntax.Model.Network.Builder.Self as Self
import           Luna.Syntax.Model.Network.Term
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.AST.Arg
import           Data.Layer
import           Data.Layer.Cover

-------------------------------------
-- === Term building utilities === --
-------------------------------------

class    ElemBuilder el m  a where buildElem :: el -> m a
instance ElemBuilder el IM a where buildElem = impossible


-- === Instances === --

instance ( SmartCons el (Uncovered a)
         , CoverConstructor m a
         , Dispatcher ELEMENT a m
         , Self.MonadSelfBuilder s m
         , Castable a s
         ) => ElemBuilder el m a where
    -- TODO[WD]: change buildAbsMe to buildMe2
    --           and fire monad every time we construct an element, not once for the graph
    buildElem el = dispatch ELEMENT =<< Self.buildAbsMe (constructCover $ Record.cons el) where
    {-# INLINE buildElem #-}



-------------------------------
-- === Term constructors === --
-------------------------------

-- Args

arg :: a -> Arg a
arg = Arg Nothing

-- Literals

star :: ElemBuilder Star m u => m u
star = buildElem Star

string :: ElemBuilder Str m u => String -> m u
string = buildElem . Str

int :: ElemBuilder Num m u => Int -> m u
int = buildElem . Num

-- Val

cons :: ElemBuilder (Cons a) m u => a -> m u
cons = buildElem . Cons

arr :: ( MonadFix m
       , ElemBuilder (Arrow (Connection a u)) m u
       , Connectible a u m
       ) => a -> a -> m u
arr src dst = mdo
    out <- buildElem $ Arrow csrc cdst
    csrc  <- connection src out
    cdst  <- connection dst out
    return out

-- Thunk

acc :: ( MonadFix m
       , ElemBuilder (Acc n (Connection a u)) m u
       , Connectible a u m
       ) => n -> a -> m u
acc name obj = mdo
    out <- buildElem $ Acc name cobj
    cobj  <- connection obj out
    return out

app :: ( MonadFix m
       , ElemBuilder (App (Connection a u)) m u
       , Connectible a u m
       ) => a -> [Arg a] -> m u
app f args = mdo
    out <- buildElem $ App cf cargs
    cf  <- connection f out
    cargs <- mapM (\(Arg n a) -> (Arg n) <$> (connection a out)) args
    return out

-- Expr

var :: ElemBuilder (Var a) m u => a -> m u
var = buildElem . Var


unify :: ( MonadFix m
         , ElemBuilder (Unify (Connection b u)) m u
         , Connectible a u m
         , Connectible b u m
         , Connection b u ~ Connection a u
         ) => a -> b -> m u
unify a b = mdo
    out <- buildElem $ Unify ca cb
    ca  <- connection a out
    cb  <- connection b out
    return out

-- Draft

blank :: ElemBuilder Blank m u => m u
blank = buildElem Blank



------------------------------
-- === Network Building === --
------------------------------

type NetLayers = '[Type, Succs, Markable]
type NetNode   = NetLayers :< Draft Static

type NetGraph = Graph (NetLayers :< Raw) (Link (NetLayers :< Raw))

buildNetwork  = runIdentity ∘ buildNetworkM
buildNetworkM = rebuildNetworkM' (def :: NetGraph)

rebuildNetwork' = runIdentity .: rebuildNetworkM'
rebuildNetworkM' (net :: NetGraph) = flip Self.evalT (undefined ::        Ref $ Node NetNode)
                                   ∘ flip Type.evalT (Nothing   :: Maybe (Ref $ Node NetNode))
                                   ∘ constrainTypeM1 CONNECTION (Proxy :: Proxy $ Ref c)
                                   ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref $ Node NetNode)
                                   ∘ flip GraphBuilder.runT net
                                   ∘ registerSuccs   CONNECTION
{-# INLINE   buildNetworkM #-}
{-# INLINE rebuildNetworkM' #-}


class NetworkBuilderT net m n | m -> n, m -> net where runNetworkBuilderT :: net -> m a -> n (a, net)

instance {-# OVERLAPPABLE #-} NetworkBuilderT I IM IM where runNetworkBuilderT = impossible
instance {-# OVERLAPPABLE #-}
    ( m      ~ Listener CONNECTION SuccRegister m'
    , m'     ~ GraphBuilder.BuilderT n e m''
    , m''    ~ Listener ELEMENT (TypeConstraint Equality_Full (Ref $ Node NetNode)) m'''
    , m'''   ~ Listener CONNECTION (TypeConstraint Equality_M1 (Ref c)) m''''
    , m''''  ~ Type.TypeBuilderT (Ref $ Node NetNode) m'''''
    , m''''' ~ Self.SelfBuilderT (Ref $ Node NetNode) m''''''
    , Monad m'''''
    , Monad m''''''
    , net ~ Graph n e
    ) => NetworkBuilderT net m m'''''' where
    runNetworkBuilderT net = flip Self.evalT (undefined ::        Ref $ Node NetNode)
                           ∘ flip Type.evalT (Nothing   :: Maybe (Ref $ Node NetNode))
                           ∘ constrainTypeM1 CONNECTION (Proxy :: Proxy $ Ref c)
                           ∘ constrainTypeEq ELEMENT    (Proxy :: Proxy $ Ref $ Node NetNode)
                           ∘ flip GraphBuilder.runT net
                           ∘ registerSuccs   CONNECTION





-- FIXME[WD]: poprawic typ oraz `WithElement_` (!)
-- FIXME[WD]: inputs should be more general and should be refactored out
inputsxxx :: forall x ast rt ls.
      ( x ~ Ref (Link (ls :< ast rt))
      , (MapTryingElemList_ (Props Variant (RecordOf (RecordOf (ast rt ls)))) (TFoldable (Ref (Link (ls :< ast rt)))) (ast rt ls))
      ) => ast rt ls -> [x]
inputsxxx a = withElement_ (p :: P (TFoldable x)) (foldrT (:) []) a




-- FIXME[WD]: poprawic typ oraz `WithElement_` (!)
-- FIXME[WD]: inputs should be more general and should be refactored out
inputsxxx2 :: forall layout term rt x.
      (MapTryingElemList_
                            (Elems term (ByRuntime rt Str x) x)
                            (TFoldable x)
                            (Term layout term rt), x ~ Layout layout term rt) => Term layout term rt -> [x]
inputsxxx2 a = withElement_ (p :: P (TFoldable x)) (foldrT (:) []) a



type instance Prop Inputs (Term layout term rt) = [Layout layout term rt]
instance (MapTryingElemList_
                           (Elems
                              term
                              (ByRuntime rt Str (Layout layout term rt))
                              (Layout layout term rt))
                           (TFoldable (Layout layout term rt))
                           (Term layout term rt)) => Getter Inputs (Term layout term rt) where getter _ = inputsxxx2
