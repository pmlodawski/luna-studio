{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RecursiveDo            #-}

module Luna.Syntax.Model.Graph.Term where

import Prologue hiding (Getter, Setter, Cons, Num, cons)

import           Control.Monad.Event
import           Data.Attribute
import           Data.Layer.Cover
import           Data.Record                    (RecordOf, IsRecord, asRecord, SmartCons)
import qualified Data.Record                    as Record
import           Data.Reprx                     (Repr, repr)
import qualified Luna.Syntax.AST.Term           as Term
import           Luna.Syntax.AST.Term           hiding (Val, Lit, Thunk, Expr, Draft)
import           Luna.Syntax.Model.Builder.Self
import           Luna.Syntax.Model.Graph.Class
import           Luna.Syntax.Model.Layer
import qualified Luna.Syntax.Model.Graph.Builder as GraphBuilder


import qualified Luna.Syntax.Model.Builder.Type as Type
import qualified Luna.Syntax.Model.Builder.Self as Self
import           Luna.Syntax.Model.Graph.Layers
import           Luna.Syntax.AST.Layout


import Data.Record (Variant, MapTryingElemList_, withElement_, Props)

import Data.Attribute

---------------------------------------
-- === Network layout definition === --
---------------------------------------

type instance Layout (Network ls) term rt = Ref $ Link (ls :< TermWrapper term rt)


---------------------------
-- === Network Terms === --
---------------------------

-- === Definitions === --

type family TermWrapper (a :: *) :: * -> [*] -> *

data    Raw      (ls :: [*]) = Raw Data                                deriving (Show, Eq)
newtype Lit   rt (ls :: [*]) = Lit   (Term (Network ls) Term.Lit   rt) deriving (Show, Eq)
newtype Val   rt (ls :: [*]) = Val   (Term (Network ls) Term.Val   rt) deriving (Show, Eq)
newtype Thunk rt (ls :: [*]) = Thunk (Term (Network ls) Term.Thunk rt) deriving (Show, Eq)
newtype Expr  rt (ls :: [*]) = Expr  (Term (Network ls) Term.Expr  rt) deriving (Show, Eq)
newtype Draft rt (ls :: [*]) = Draft (Term (Network ls) Term.Draft rt) deriving (Show, Eq)


-- === Instances === --

-- Wrappers

makeWrapped ''Raw
makeWrapped ''Lit
makeWrapped ''Val
makeWrapped ''Thunk
makeWrapped ''Expr
makeWrapped ''Draft

type instance Unlayered (Raw      ls) = Unwrapped (Raw      ls)
type instance Unlayered (Lit   rt ls) = Unwrapped (Lit   rt ls)
type instance Unlayered (Val   rt ls) = Unwrapped (Val   rt ls)
type instance Unlayered (Thunk rt ls) = Unwrapped (Thunk rt ls)
type instance Unlayered (Expr  rt ls) = Unwrapped (Expr  rt ls)
type instance Unlayered (Draft rt ls) = Unwrapped (Draft rt ls)

instance Layered (Raw      ls)
instance Layered (Lit   rt ls)
instance Layered (Val   rt ls)
instance Layered (Thunk rt ls)
instance Layered (Expr  rt ls)
instance Layered (Draft rt ls)

-- Layout types

type instance LayoutType (Raw      ls) = Network ls
type instance LayoutType (Lit   rt ls) = Network ls
type instance LayoutType (Val   rt ls) = Network ls
type instance LayoutType (Thunk rt ls) = Network ls
type instance LayoutType (Expr  rt ls) = Network ls
type instance LayoutType (Draft rt ls) = Network ls

-- Term bindings

type instance TermWrapper Term.Lit   = Lit
type instance TermWrapper Term.Val   = Val
type instance TermWrapper Term.Thunk = Thunk
type instance TermWrapper Term.Expr  = Expr
type instance TermWrapper Term.Draft = Draft

-- Records

type instance RecordOf (Lit   rt ls) = RecordOf (Unwrapped (Lit   rt ls))
type instance RecordOf (Val   rt ls) = RecordOf (Unwrapped (Val   rt ls))
type instance RecordOf (Thunk rt ls) = RecordOf (Unwrapped (Thunk rt ls))
type instance RecordOf (Expr  rt ls) = RecordOf (Unwrapped (Expr  rt ls))
type instance RecordOf (Draft rt ls) = RecordOf (Unwrapped (Draft rt ls))

instance IsRecord (Lit   rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Val   rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Thunk rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Expr  rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Draft rt ls) where asRecord = wrapped' ∘ asRecord

-- Conversions

instance Castable (Raw      ls) (Raw      ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Lit   rt ls) (Lit   rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Val   rt ls) (Val   rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Thunk rt ls) (Thunk rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Expr  rt ls) (Expr  rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Draft rt ls) (Draft rt ls) where cast = id ; {-# INLINE cast #-}

instance Castable (Lit   rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Val   rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Thunk rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Expr  rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Draft rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}

instance Castable (Raw ls) (Lit   rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Val   rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Thunk rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Expr  rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Draft rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}

-- Representations

instance {-# OVERLAPPABLE #-}                                     Repr s (Raw      ls) where repr = const "Raw"
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Lit   rt ls)) => Repr s (Lit   rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Val   rt ls)) => Repr s (Val   rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Thunk rt ls)) => Repr s (Thunk rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Expr  rt ls)) => Repr s (Expr  rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Draft rt ls)) => Repr s (Draft rt ls) where repr = repr ∘ unwrap'

-- Attributes

type instance Attr a (Lit   rt ls) = Attr a (Unwrapped (Lit   rt ls))
type instance Attr a (Val   rt ls) = Attr a (Unwrapped (Val   rt ls))
type instance Attr a (Thunk rt ls) = Attr a (Unwrapped (Thunk rt ls))
type instance Attr a (Expr  rt ls) = Attr a (Unwrapped (Expr  rt ls))
type instance Attr a (Draft rt ls) = Attr a (Unwrapped (Draft rt ls))

instance SubGetter a (Lit   rt ls) => Getter a (Lit   rt ls)
instance SubGetter a (Val   rt ls) => Getter a (Val   rt ls)
instance SubGetter a (Thunk rt ls) => Getter a (Thunk rt ls)
instance SubGetter a (Expr  rt ls) => Getter a (Expr  rt ls)
instance SubGetter a (Draft rt ls) => Getter a (Draft rt ls)

instance SubSetter a (Lit   rt ls) => Setter a (Lit   rt ls)
instance SubSetter a (Val   rt ls) => Setter a (Val   rt ls)
instance SubSetter a (Thunk rt ls) => Setter a (Thunk rt ls)
instance SubSetter a (Expr  rt ls) => Setter a (Expr  rt ls)
instance SubSetter a (Draft rt ls) => Setter a (Draft rt ls)



-------------------------------------
-- === Term building utilities === --
-------------------------------------

class    ElemBuilder el m  a where buildElem :: el -> m a
instance ElemBuilder el IM a where buildElem = impossible


-- === Instances === --

instance ( SmartCons el (Uncovered a)
         , CoverConstructor m a
         , Dispatcher ELEMENT a m
         , MonadSelfBuilder s m
         , Castable a s
         ) => ElemBuilder el m a where
    -- TODO[WD]: change buildAbsMe to buildMe2
    --           and fire monad every time we construct an element, not once for the graph
    buildElem el = dispatch ELEMENT =<< buildAbsMe (constructCover $ Record.cons el) where
    {-# INLINE buildElem #-}



-------------------------------
-- === Term constructors === --
-------------------------------

arg :: a -> Arg a
arg = Arg Nothing

fromArg :: Arg a -> a
fromArg (Arg _ a) = a

star :: ElemBuilder Star m u => m u
star = buildElem Star

string :: ElemBuilder Str m u => String -> m u
string = buildElem . Str

int :: ElemBuilder Num m u => Int -> m u
int = buildElem . Num

var :: ElemBuilder (Var a) m u => a -> m u
var = buildElem . Var

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

blank :: ElemBuilder Blank m u => m u
blank = buildElem Blank

------------------------------
-- === Network Building === --
------------------------------

type NetLayers = '[Type, Succs]
type NetNode   = NetLayers :< Draft Static

type NetGraph = Graph (NetLayers :< Raw) (Link (NetLayers :< Raw))

buildNetwork  = runIdentity ∘ buildNetworkM
buildNetworkM = rebuildNetworkM' (def :: NetGraph)
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
                            (Elems term (ByLayout rt Str x) x)
                            (TFoldable x)
                            (Term layout term rt), x ~ Layout layout term rt) => Term layout term rt -> [x]
inputsxxx2 a = withElement_ (p :: P (TFoldable x)) (foldrT (:) []) a



type instance Attr Inputs (Term layout term rt) = [Layout layout term rt]
instance (MapTryingElemList_
                           (Elems
                              term
                              (ByLayout rt Str (Layout layout term rt))
                              (Layout layout term rt))
                           (TFoldable (Layout layout term rt))
                           (Term layout term rt)) => Getter Inputs (Term layout term rt) where getter _ = inputsxxx2

