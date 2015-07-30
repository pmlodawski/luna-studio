
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}


module Main where

import Flowbox.Prelude hiding (simple, empty, Indexable, Simple, cons)
import Data.Repr

--import qualified Luna.Inference.Type as Type
--import           Luna.Inference.Type
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Text.Lazy      as Text
import           Data.Text.Lazy      (Text)

--import           Luna.Inference.RawData



import           GHC.Prim (Any)
import           Unsafe.Coerce (unsafeCoerce)
import           Data.Convert

--import qualified Data.Graph.Inductive as Graph
--import           FastString (FastString, mkFastString, unpackFS)


import qualified Data.IntMap.Lazy as IntMap
import           Data.IntMap.Lazy (IntMap)
import Data.Typeable       hiding (cast)
import Control.Monad.State hiding (withState, mapM)


import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete hiding (Label)
import qualified Data.GraphViz.Attributes.Complete as GV
import Data.GraphViz.Printing (toDot)
import Data.GraphViz.Commands

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable ()
import Data.Maybe (fromJust)

import Data.Indexable

import Data.Containers

import System.Process

import qualified Data.Text.AutoBuilder as Text

import Data.Map (Map)
import qualified Data.Map as Map
--add :: Int -> Int -> Int
--add = (+)

--add :: (Int, (Int, ())) -> Int
--add (a, (b, ())) = a + b


--testf :: (Int, (String, ())) -> (Int,String)
--testf (a,(b,())) = (a,b)

--unpackTest :: Object -> (Int,String)
--unpackTest = unpackRawData


--unpackInt :: Object -> Int
--unpackInt = unpackRawData

--f = unsafeCoerce add :: Any

--appx :: a -> Any -> Any
--appx a f = (unsafeCoerce f :: Any -> Any) (unsafeCoerce a :: Any)

--instance Default (Graph.Gr a b) where
--    def = Graph.mkGraph def def

--empty = def :: Graph.Gr Node ()


--instance IsString FastString where
--    fromString = mkFastString

type ID = Int

--data Expr = Input    FastString
--          | Accessor FastString
--          | Cons     FastString
--          | App
--          deriving (Show)


--data AST = ASTExpr Expr
--         deriving (Show)



-- === Literals ===

data Literal = Int Int
             | String Text.AutoBuilder


instance IsString Literal where
    fromString = String . fromString


-- === Expr ===

data Key t = Key { fromKey :: ID } deriving (Show) -- { overKey :: Lens' Graph (Maybe t) }

--makeLenses ''Key


type Name = String


type HExpr h = h (Expr h)


--h (Expr h) , h = TExpr => TExpr (Expr TExpr) === Ptr Int (TExpr Type (Expr TExpr))

data AST h = ASTExpr (h Expr)
         --deriving (Show)
data Node = Node


-- === Expr ===
data Arg h = Arg { _label :: Maybe Name, _val :: HExpr h }

-- wszystkie rzeczy nazwane powinny byc w slownikach w jezyku! - czyli datatypy ponizej zostaja,
-- ale mozemy tworzyc np. Var funkcjami, ktore oczekuja konkretnego Stringa!
data Expr h = Var      Name
            | Cons     Name
            | Accessor Name (HExpr h)
            | App      (HExpr h) [Arg h]
            | Lambda
            | RecUpd
            | Match    (Pattern h) (HExpr h)
            | Case
            | Typed
            -- | Assignment
            | Decons
            | Curry
            -- | Meta
            -- | Tuple
            -- | Grouped
            -- | Decl
            | Lit
            | Wildcard
            -- | Tuple
            -- | List

type Pattern h = HExpr h
--data Pattern h = PCons (HExpr h) [Arg h]
--               | PVar  (HExpr h) -- do tego HExpra laczymy w grafie inne edge!


deriving instance Show (HExpr h) => Show (Arg h)
deriving instance Show (HExpr h) => Show (Expr h)
--deriving instance Show (HExpr h) => Show (Pattern h)

makeLenses ''Expr
makeLenses ''Arg


instance Repr (Expr h) where
    repr = \case
        Var      n   -> "Var "      <> show n
        Cons     n   -> "Cons "     <> show n
        Accessor n _ -> "Accessor " <> show n
        App      {}  -> "App"

--type Foo1 i = Expr (TPtr i)
--type Foo2 i = Expr (Ptr i)


-- === Ptr ===


data Typex = Typex -- fixme
data Label l a = Label l a

data    Tp     a = Tp Typex a


newtype Ptr  i a = Ptr  i

class    IsPtr p i | p -> i where mkPtr :: i -> p a
instance IsPtr (Ptr i) i    where mkPtr = Ptr


instance (Typeable a, Show i) => Show (Ptr i a) where
    show (Ptr i) = "Ptr " <> show i <> " (" <> show (typeOf (undefined :: a)) <> ")"

class    HasPtr t         i a | t -> i a where ptr :: Lens' t (Ptr i a)
instance HasPtr (Ptr i a) i a            where ptr = id




--instance IsPtr TPtr where mkPtr = TPtr . Ptr


---------- ?


unsafeAt :: (UncheckedGetIdx m, UncheckedSetIdx m) => Index m -> Lens' m (ElementOf m)
unsafeAt i = lens (uncheckedGetIdx i) (flip $ uncheckedSetIdx i)

mapOver :: (Lens' b a) -> (a -> (a, x)) -> (b -> (b, x))
mapOver lens f s = (s & lens .~ a, out) where
    (a, out) = f $ s ^. lens







class IsNode n inp | n -> inp where
    inputs :: n -> [inp]

instance IsNode (Expr h) (HExpr h) where
    inputs = \case
        Accessor _    base -> [base]
        App      base args -> base : fmap (view val) args
        _                  -> []







--- === [TOEXTRACT] Instances ===
instance Default (Vector a) where def = mempty





--- === Hidden ===

type NodeVal a = (Show a, Typeable a, Repr a)

data Hidden where
    Hidden :: NodeVal a => a -> Hidden



castableLens :: IsoCastable a b => Iso' a b
castableLens = iso cast cast

-- instances

instance Show Hidden where show (Hidden a) = show a
instance Repr Hidden where repr (Hidden a) = repr a

instance {-# OVERLAPPABLE #-} Castable Hidden a where
    cast (Hidden a) = unsafeCoerce a

instance {-# OVERLAPPABLE #-} NodeVal a => Castable a Hidden where
    cast = Hidden




--- === HContainer ===

data HContainer elems = HContainer { _elems :: elems }

makeLenses ''HContainer

class HasHContainer a n | a -> n where
    hContainer :: Lens' a (HContainer n)


type OverHContainer m ptr a = (UncheckedSetIdx m, UncheckedGetIdx m, IsoCastable a (ElementOf m), HasPtr ptr (Index m) a)

unsafeOverPtr :: OverHContainer m ptr a => ptr -> Lens' (HContainer m) a
unsafeOverPtr (view ptr -> Ptr i) = elems . unsafeAt i . castableLens

unsafeSet :: OverHContainer m ptr a => ptr -> a -> HContainer m -> HContainer m
unsafeSet ptr a = unsafeOverPtr ptr .~ a

unsafeGet :: OverHContainer m ptr a => ptr -> HContainer m -> a
unsafeGet ptr = view $ unsafeOverPtr ptr

insert :: (Appendable elems, Indexable elems, Castable a (ElementOf elems), IsPtr ptr (Index elems))
       => a -> HContainer elems -> (HContainer elems, ptr a)
insert a g = (g', mkPtr . lastIdx $ g' ^. elems) where
    g' = g & elems %~ (flip append $ cast a)




-- FIXME (unsafe implementation)
inputs' :: HContainer (Vector Hidden) -> ID -> [Ptr Int (Expr (Ptr Int))]
inputs' g id = inputs $ unsafeGet (Ptr id :: Ptr Int (Expr (Ptr Int))) g

-- instances

deriving instance Show elems => Show (HContainer elems)

instance Default elems => Default (HContainer elems) where
    def = HContainer def



-- === Node ===

data Type a = Type a
            | Star

--data Node a = Node { _tp :: Type a, _nodeval :: Value a }

data Value a = Val  a
             | BVal -- binary value



--- === Builder ===

newtype NodePtr  a = NodePtr  { __ptr  :: Ptr Int a      } deriving (Show)
newtype NodeTPtr a = NodeTPtr { __tptr :: Ptr Int (Tp a) } deriving (Show)

makeLenses ''NodePtr
makeLenses ''NodeTPtr

instance HasPtr (NodePtr a)  Int a      where ptr = _ptr
instance HasPtr (NodeTPtr a) Int (Tp a) where ptr = _tptr

instance IsPtr NodePtr  Int where mkPtr = NodePtr  . Ptr
instance IsPtr NodeTPtr Int where mkPtr = NodeTPtr . Ptr

--

newtype Graph = Graph { _reg :: HContainer (Vector Hidden) } deriving (Show, Default)

makeClassy ''Graph


type GraphBuilder m = StateT BldrState m

data BldrState = BldrState { _orphans :: [Int]
                           , __graph  :: Graph
                           }

makeClassy ''BldrState


class Monad m => MonadBldrState m where
    getBldrState :: m BldrState
    putBldrState :: BldrState -> m ()


instance Default BldrState where
    def = BldrState def def

instance HasGraph BldrState where graph = _graph

instance Monad m => MonadBldrState (GraphBuilder m) where
    getBldrState = get
    putBldrState = put

instance (IsPtr ptr Int, Show (a ptr), Typeable a, Typeable ptr, Monad m)
      => RefBuilder (GraphBuilder m) ptr a where
    mkRef a = fmap Ref . withGraph . mapOver reg $ insert a

-- utils

withBldrState_ :: MonadBldrState m => (BldrState -> BldrState) -> m ()
withBldrState_ f = withBldrState $ fmap (,()) f


withBldrState :: MonadBldrState m => (BldrState -> (BldrState, a)) -> m a
withBldrState f = do
    bldr <- getBldrState
    let (bldr', out) = f bldr
    putBldrState $ bldr'
    return out


withGraph :: MonadBldrState m => (Graph -> (Graph, a)) -> m a
withGraph = withBldrState . mapOver graph

withOrphans :: MonadBldrState m => ([Int] -> (a, [Int])) -> m a
withOrphans f = do
    s <- getBldrState
    let orphLens    = bldrState . orphans
        (a, norphs) = f (s ^. orphLens)
    putBldrState $ s & orphLens .~ norphs
    return a

popOrphan :: MonadBldrState m => m (Maybe Int)
popOrphan = withOrphans $ \case
    []         -> (Nothing, [])
    (id : ids) -> (Just id, ids)



---- === Ref ===

newtype Ref       h a = Ref     { fromRef    :: h (a h) }
newtype RefCons m h a = RefCons { runRefCons :: m (Ref h a) }

class ToMRef t m h a | t -> h a where
    toMRef :: t -> m (Ref h a)

instance                      (m ~ n, Monad m) => ToMRef    (RefCons m h a) n h a where toMRef = runRefCons
instance                             (Monad m) => ToMRef    (Ref h a)        m h a where toMRef = return
instance {-# OVERLAPPABLE #-} (m ~ n, Monad m) => ToMRef (m (Ref h a))       n h a where toMRef = id

deriving instance Show (h (a h)) => Show (Ref h a)

---- === RefBuilder ===

class Monad m => RefBuilder m h a where
    mkRef :: a h -> m (Ref h a)

class Monad m => RefHandler m h a | m -> h a where
    registerRef :: Name -> Ref h a -> m ()

type ASTBuilder m h a = (RefBuilder m h a, RefHandler m h a)

--

mkCons :: RefBuilder m h a => a h -> RefCons m h a
mkCons = RefCons . mkRef

var :: RefBuilder m h Expr => Name -> RefCons m h Expr
var = mkCons . Var

cons :: RefBuilder m h Expr => Name -> RefCons m h Expr
cons = mkCons . Cons

ref :: (RefHandler m h a, ToMRef t m h a) => Name -> t -> m (Ref h a)
ref name t = do
    ref <- toMRef t
    registerRef name ref
    return ref

mrefRaw :: (ToMRef t m h a, Functor m) => t -> m (h (a h))
mrefRaw = fmap fromRef . toMRef

accessor :: (RefBuilder m h Expr, ToMRef t m h Expr) => Name -> t -> RefCons m h Expr
accessor name el = RefCons $ mkRef . Accessor name =<< mrefRaw el


match :: (ToMRef t1 m h Expr, ToMRef t2 m h Expr, RefBuilder m h Expr, Monad m)
      => t1 -> t2 -> RefCons m h Expr
match a b = RefCons $ do
    ra <- mrefRaw a
    rb <- mrefRaw b
    mkRef $ Match ra rb



---- === RefBuilder ===


--def foo [x,y] : ...

data Function body = Function { -- _args   :: [Name] -- tu powinny byc patterny
                               _body   :: body
                              , _fgraph :: Graph
                              } deriving (Show)

type NodeFunction ptr = Function (GFBody ptr)

type GFBody ptr = Map String (Ref ptr Expr)

instance Default body => Default (Function body) where def = Function def def

makeLenses ''Function
---- === Examples ===


runGraphBuilder :: Monad m => GraphBuilder m a -> m Graph
runGraphBuilder gb = view graph <$> execStateT gb def

runFunctionBuilder :: (Default body, Monad m) => FunctionBuilder body m a -> m (a, Function body)
runFunctionBuilder fb = runStateT fb def

type FunctionBuilder body m = StateT (Function body) m

class MonadFunctionBuilder body m | m -> body where
    getFunction :: m (Function body)
    putFunction :: Function body -> m ()

instance Monad m => MonadFunctionBuilder body (FunctionBuilder body m) where
    getFunction = get
    putFunction = put


--class Monad m => RefHandler m h a | m -> h a where
--    registerRef :: Name -> Ref h a -> m ()

instance {-# OVERLAPPABLE #-} (MonadTrans t, RefHandler m h a, Monad (t m)) => RefHandler (t m) h a where
    registerRef name ref = lift $ registerRef name ref

instance Monad m => RefHandler (FunctionBuilder (GFBody h) m) h Expr where
    registerRef name ref = do
        func <- getFunction
        putFunction $ func & body %~ Map.insert name ref


--foo :: _
buildFunction desc = f & fgraph .~ g where
    (g,f) = runIdentity $ runFunctionBuilder $ runGraphBuilder desc


--tst :: (Graph, Function (GFBody NodePtr))
fck :: Function (GFBody NodePtr)
fck = buildFunction g1

g1 :: ASTBuilder m NodePtr Expr => m (Ref NodePtr Expr)
g1 = do
    a    <- ref "foo" $ var "a"
    b    <- ref "bar" $ accessor "foo" (var "b")
    c    <- ref "baz" $ match (var "a") (var "b")
    return a
    --mod  <- var "Main"
    --foo  <- a    @.  "foo"
    --b    <- foo  @$$ [a]
    --bar  <- mod  @.  "bar"
    --c    <- bar  @$$ [b]
    --plus <- mod  @.  "plus"
    --out  <- plus @$$ [c, a]
    --return ()

main = do
    let m = fck & view body
        g = fck & view fgraph
        Just (Ref p1) = Map.lookup "foo" m
        r = g ^. reg
        e = unsafeGet p1 r

    print fck
    print e


----g1 :: RefBuilder Expr m => m ()
--g1 = do
--    a    <- ref "a" $ var "a"
--    mod  <- ref "mod"  $ cons "Mod"
--    foo  <- ref "foo"  $ a    @. "foo"
--    b    <- ref "b"    $ foo  @$ [arg a]
--    bar  <- ref "bar"  $ mod  @. "bar"
--    c    <- ref "c"    $ bar  @$ [arg b]
--    plus <- ref "plus" $ mod  @. "plus"
--    out  <- ref "out"  $ plus @$ [arg c, arg a]
--    return ()


--class ToMRef3 t where
--    toMRef3 :: Monad m => t m a -> m (Ref m a)

--instance ToMRef3 RefCons where toMRef3 = runRefCons
--instance ToMRef3 Ref      where toMRef3 = return




--instance MonadBldrState c (State (BldrState c)) where
--    getBldrState = get
--    putBldrState = put



--runNodeBuilder :: Builder (Vector Hidden) a -> Graph (Vector Hidden)
--runNodeBuilder = view graph . flip execState def


----g2 :: Graph (Vector Hidden)
----g2 = runNodeBuilder $ do
----    a    <- var "a"
----    mod  <- var "Main"
----    foo  <- a    @.  "foo"
----    b    <- foo  @$$ [a]
----    bar  <- mod  @.  "bar"
----    c    <- bar  @$$ [b]
----    plus <- mod  @.  "plus"
----    out  <- plus @$$ [c, a]
----    return ()

----data Arg h = Arg { _label :: Maybe Name, _val :: HExpr h }


----class Arg2 a where
----    arg2 :: a -> b

----instance Arg2 Name -> (a -> Arg a)

--class Named a where
--    named :: Name -> a -> a

--instance Named (ArgRef m a) where
--    named n (ArgRef _ ref) = ArgRef (Just n) ref

--data ArgRef m a = ArgRef (Maybe Name) (m (Ref m a))

--arg = ArgRef Nothing . toMRef2

----data Node = Node


--g2 :: (ConvertibleM Expr a, Monad m, RefBuilder a m) => m (Ref m a)
--g2 = do
--    a <- ref2_ $ var2 "a"
--    return a

----g1 :: RefBuilder Expr m => m ()
--g1 = do
--    a    <- ref "a" $ var "a"
--    mod  <- ref "mod"  $ cons "Mod"
--    foo  <- ref "foo"  $ a    @. "foo"
--    b    <- ref "b"    $ foo  @$ [arg a]
--    bar  <- ref "bar"  $ mod  @. "bar"
--    c    <- ref "c"    $ bar  @$ [arg b]
--    plus <- ref "plus" $ mod  @. "plus"
--    out  <- ref "out"  $ plus @$ [arg c, arg a]
--    return ()

----dorobic budowanie liniowe, nie grafowe
----zmienne powinny odnosic sie do siebie jako debrouigle - moze warto parametryzowac Name, tak y mozna bylo wsadzic tam ID debrouiglowe?

--(@.)  = access
--(@$)  = app

--main = do
--    let g  = runNodeBuilder g1
--        gv = toGraphViz g
--    print g
--    --print $ toDot gv
--    runGraphviz gv Png "/tmp/out.png"
--    createProcess $ shell "open /tmp/out.png"
--    print "end"


--data Label a e = Label a e
--type Labeled l a = Label l (a (Label l))
--type LExpr l = Labeled l Expr

--newtype Simple a = Simple a deriving (Show)
--type SExpr = Simple (Expr Simple)

----newtype Mu f = Mu (f (Mu f))


----inputs' :: Graph (Vector Hidden) -> ID -> [Ptr Int (Expr (Ptr Int))]

--toGraphViz :: Graph (Vector Hidden) -> DotGraph Int
--toGraphViz g = DotGraph { strictGraph     = False
--                        , directedGraph   = True
--                        , graphID         = Nothing
--                        , graphStatements = DotStmts { attrStmts = []
--                                                     , subGraphs = []
--                                                     , nodeStmts = nodeStmts
--                                                     , edgeStmts = edgeStmts
--                                                     }
--                        }
--    where ns              = g ^. nodes
--          nodeIds         = [0 .. Vector.length ns - 1] :: [Int]
--          elems           = fmap ((Vector.!) ns) nodeIds
--          nodeLabels      = fmap repr elems
--          labeledNode s a = DotNode a [GV.Label . StrLabel $ fromString s]
--          nodeStmts       = fmap (uncurry labeledNode) $ zip nodeLabels nodeIds
--          nodeInEdges   n = zip3 [0..] (fmap fromPtr $ inputs' g n) (repeat n)
--          inEdges         = concat $ fmap nodeInEdges nodeIds
--          mkEdge  (n,a,b) = DotEdge a b [GV.Label . StrLabel $ fromString $ show n]
--          edgeStmts       = fmap mkEdge inEdges





