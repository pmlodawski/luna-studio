
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

import           Luna.Inference.RawData

import           Luna.Inference.Function

import           GHC.Prim (Any)
import           Unsafe.Coerce (unsafeCoerce)
import           Data.Convert

import qualified Data.Graph.Inductive as Graph
import           FastString (FastString, mkFastString, unpackFS)


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


instance IsString FastString where
    fromString = mkFastString

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


type Name = FastString


type HExpr h = h (Expr h)


--h (Expr h) , h = TExpr => TExpr (Expr TExpr) === Ptr Int (TExpr Type (Expr TExpr))

data AST h = ASTExpr (h Expr)
         --deriving (Show)
data Node = Node


-- === Expr ===
data Arg h = Arg { _label :: Maybe Name, _val :: HExpr h }

-- wszystkie rzeczy nazwane powinny byc w slownikach w jezyku! - czyli datatypy ponizej zostaja,
-- ale mozemy tworzyc np. Var funkcjami, ktore oczekuja konkretnego Stringa!
data Expr h = Var      { _name :: Name                                                      }
            | Cons     { _name :: Name                                                      }
            | Accessor { _name :: Name, _base :: HExpr h                            }
            | App      {                _base :: HExpr h, _args :: [Arg h]            }
            | Lambda
            | RecUpd
            | Case
            | Typed
            -- | Assignment
            | Decons
            | Curry
            -- | Meta
            -- | Tuple
            -- | Grouped
            | Decl
            | Lit
            | Native
            | Wildcard
            -- | Tuple
            -- | List



deriving instance Show (HExpr h) => Show (Arg h)
deriving instance Show (HExpr h) => Show (Expr h)

makeLenses ''Expr
makeLenses ''Arg


instance Repr (Expr h) where
    repr = \case
        Var      n   -> "Var "      <> show (unpackFS n)
        Cons     n   -> "Cons "     <> show (unpackFS n)
        Accessor n _ -> "Accessor " <> show (unpackFS n)
        App      {}  -> "App"

--type Foo1 i = Expr (TPtr i)
--type Foo2 i = Expr (Ptr i)


-- === Ptr ===


data Typex = Typex -- fixme
data Label l a = Label l a

data    Tp     a = Tp Typex a


newtype Ptr  i a = Ptr  i
newtype TPtr i a = TPtr { __ptr :: Ptr i (Tp a) } deriving (Show)

instance (Typeable a, Show i) => Show (Ptr i a) where
    show (Ptr i) = "Ptr \"" <> show (typeOf (undefined :: a)) <> "\" " <> show i

makeLenses ''TPtr


class HasPtr t i a | t -> i a where
    ptr :: Lens' t (Ptr i a)

instance HasPtr (Ptr i a) i a where
    ptr = id

instance HasPtr (TPtr i a) i (Tp a) where
    ptr = _ptr


class FromPtr i a p | p -> i a where
    fromPtr :: Ptr i a -> p

instance FromPtr i a      (Ptr i a)  where fromPtr = id
instance FromPtr i (Tp a) (TPtr i a) where fromPtr = TPtr



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



hiddenLens :: IsoCastable a b => Iso' a b
hiddenLens = iso cast cast

-- instances

instance Show Hidden where show (Hidden a) = show a
instance Repr Hidden where repr (Hidden a) = repr a

instance {-# OVERLAPPABLE #-} Castable Hidden a where
    cast (Hidden a) = unsafeCoerce a

instance {-# OVERLAPPABLE #-} NodeVal a => Castable a Hidden where
    cast = Hidden




--- === Graph ===

data Graph nodes = Graph { _nodes      :: nodes
                         , _argNames   :: [Name]
                         }

makeLenses ''Graph

class HasGraph a n | a -> n where
    graph :: Lens' a (Graph n)


type OverGraph m ptr a = (UncheckedSetIdx m, UncheckedGetIdx m, IsoCastable a (ElementOf m), HasPtr ptr (Index m) a)

unsafeOverPtr :: OverGraph m ptr a => ptr -> Lens' (Graph m) a
unsafeOverPtr (view ptr -> Ptr i) = nodes . unsafeAt i . hiddenLens

unsafeSet :: OverGraph m ptr a => ptr -> a -> Graph m -> Graph m
unsafeSet ptr a = unsafeOverPtr ptr .~ a

unsafeGet :: OverGraph m ptr a => ptr -> Graph m -> a
unsafeGet ptr = view $ unsafeOverPtr ptr

insert :: (Appendable nodes, Indexable nodes, Castable a (ElementOf nodes), FromPtr (Index nodes) a t)
       => a -> Graph nodes -> (Graph nodes, t)
insert a g = (g', mkPtr . lastIdx $ g' ^. nodes) where
    g' = g & nodes %~ (flip append (cast a))


mkPtr :: FromPtr i a b => i -> b
mkPtr = fromPtr . Ptr

-- FIXME (unsafe implementation)
inputs' :: Graph (Vector Hidden) -> ID -> [Ptr Int (Expr (Ptr Int))]
inputs' g id = inputs $ unsafeGet (Ptr id :: Ptr Int (Expr (Ptr Int))) g

-- instances

deriving instance Show nodes => Show (Graph nodes)

instance Default nodes => Default (Graph nodes) where
    def = Graph def def



-- === Node ===

data Type a = Type a
            | Star

--data Node a = Node { _tp :: Type a, _nodeval :: Value a }

data Value a = Val  a
             | BVal -- binary value



--- === Builder ===

newtype PolyContainer c a = PolyContainer (c a)

type Builder c = State (NodeGraph c)

data NodeGraph c = NodeGraph { _orphans :: [Index c]
                             , __graph  :: Graph c
                             }

makeLenses ''NodeGraph

--instance Default (Graph c) => Default (NodeGraph c) where
--    def = NodeGraph def def

--instance HasGraph (NodeGraph c) c where
--    graph = _graph

--class Monad m => MonadNodeGraph c m | m -> c where
--    getNodeGraph :: m (NodeGraph c)
--    putNodeGraph :: NodeGraph c -> m ()


--class HasNodeGraph a c | a -> c where
--    nodeGraph :: Lens' a (NodeGraph c)

--instance HasNodeGraph (NodeGraph c) c where
--    nodeGraph = id

---- utils

--withNodeGraph_ :: MonadNodeGraph c m => (NodeGraph c -> NodeGraph c) -> m ()
--withNodeGraph_ f = withNodeGraph $ fmap (,()) f


--withNodeGraph :: MonadNodeGraph c m => (NodeGraph c -> (NodeGraph c, a)) -> m a
--withNodeGraph f = do
--    bldr <- getNodeGraph
--    let (bldr', out) = f bldr
--    putNodeGraph $ bldr'
--    return out

--withGraph :: MonadNodeGraph c m => (Graph c -> (Graph c, a)) -> m a
--withGraph = withNodeGraph . mapOver graph

----withOrphans ::
--withOrphans f = do
--    s <- getNodeGraph
--    let orphLens    = nodeGraph . orphans
--        (a, norphs) = f (s ^. orphLens)
--    putNodeGraph $ s & orphLens .~ norphs
--    return a

--requestNodeID :: MonadNodeGraph c m => m (Maybe ID)
--requestNodeID = withOrphans $ \case
--    []         -> (Nothing, [])
--    (id : ids) -> (Just id, ids)

---- fixme: we should delete the elements
----releaseNodeID :: MonadNodeGraph c m => ID -> m ()
----releaseNodeID id = withNodeGraph_ (nodeScope %~ (id:))

------add :: (Appendable c, Indexable c, Castable a (ElementOf c), MonadNodeGraph c m)
------    => a -> m (Ptr (Index c) a)
------add = withGraph . insert



---- === ASTBuilder ===

--class Monad m => ASTBuilder a m where
--    mkRef :: ASTNode m a -> m (Ref m a)

--class Monad m => ASTBuilder3 a m where
--    mkRef3 :: ASTNode3 m a -> m (Ref3 m a)



----class ASTData a where
--    --type family DataLayout (a :: (* -> *) -> *) (h :: * -> *) :: *

--    ----type Bar a = DataLayout a
--    --type instance DataLayout Expr h = Expr h

--    --type ASTNode3 m a = DataLayout a (ConnectionType m)

--type family Foo (a :: (* -> *) -> *) (h :: * -> *) :: * -> *

--type instance Foo Expr h = h

----data NodeObject
----zmieniamy astnode w taki sposob by mozna bylo polimorficznie je dostawac.
----Moze zamiast parametryzowac m -> NodeType (tak jak node z typem) powinnismy m w to nie mieszac,
----bo grqf builder to graf builder i zwracac polimorficzny wynik ?

--type ASTNode m a = a (ConnectionType m)

--type ASTNode3 m a = a (Foo a (ConnectionType m))
----type Ref m a = m (ConnectionType m (a (ConnectionType m)))
--newtype Ref m a = Ref { fromRef :: ConnectionType m (ASTNode m a) }

--newtype Ref3 m a = Ref3 { fromRef3 :: (ConnectionType m) (ASTNode3 m a) }

--newtype Ref4 m a = Ref4 { fromRef4 :: (ConnectionType m) (a (ConnectionType m)) }

--type BuilderMonad c a = (Appendable c, Indexable c, Castable (a (Ptr (Index c))) (ElementOf c))


----type GraphPtr m a = Ptr (Index m) a

----insert :: (Castable a (ElementOf m), Appendable m, Indexable m)
----       => a -> Graph m -> (Graph m, GraphPtr m a)
----insert a g = (g', Ptr . lastIdx $ g' ^. nodes) where
----    g' = g & nodes %~ (flip append (cast a))


----withGraph :: MonadNodeGraph c m => (Graph c -> (Graph c, a)) -> m a
----withGraph = withNodeGraph . mapOver graph

--instance (BuilderMonad c a) => ASTBuilder a (Builder c) where
--    mkRef = fmap Ref . withGraph . insert

--instance (Appendable c, Indexable c, Castable (ASTNode3 (Builder c) a) (ElementOf c)) => ASTBuilder3 a (Builder c) where
--    mkRef3 = fmap Ref3 . withGraph . insert

----add' :: el (ConnectionType bldr) -> bldr
----add' = add

----type X =

----instance (Castable a (ElementOf c), idx ~ Index c, Appendable c, Indexable c)
----      => ASTBuilder a (Builder idx c (Ptr idx a)) where
----    add' = add

---- instances

----Expr (Ptr Int)

----data Nu f=forall a.Nu a (a->f a)
----newtype Mu f = Mu (f (Mu f))

----a = undefined :: Mu ()



----foo :: State (NodeGraph (Vector Hidden)) (Ptr Int (Expr (Ptr Int)))
----foo :: (Appendable c, LastIdx c, SetIdx c, GetIdx c,
----                       CheckedSetIdx c, CheckedGetIdx c, UncheckedSetIdx c,
----                       UncheckedGetIdx c, Castable (Expr h) (ElementOf c),
----                       MonadNodeGraph c m) =>
----                      m (Ptr (Index c) (Expr h))

----type ConvertibleNode a m b = Convertible (a (ConnectionType m)) (b (ConnectionType m))

--newtype NodeCons m a = NodeCons { runNodeCons :: m (Ref m a) }
--newtype NodeCons3 m a = NodeCons3 { runNodeCons3 :: m (Ref3 m a) }


--returnClone :: Monad m => m a -> m (m a)
--returnClone = return

--refToCons :: Monad m => m (Ref m a) -> m (NodeCons m a)
--refToCons = return . NodeCons

----refToCons2 :: m (Ref m a) -> NodeCons m a
----refToCons2 = NodeCons


--mkCons = NodeCons . mkRef
--mkCons3 = NodeCons3 . mkRef3

----accessor name el = mkRef $ Accessor name el
--access = flip accessor

--accessor name el = mkCons . Accessor name <$> mrefRaw el

--accessor2 :: (ASTBuilder Expr m, ToMRef2 t m Expr) => Name -> t -> NodeCons m Expr
--accessor2 name el = NodeCons $ mkRef . Accessor name =<< mrefRaw el

----testx :: _
----testx name el = return . convertM . Accessor name =<< mrefRaw3 el


--mrefRaw = fmap fromRef . toMRef2

--mrefRaw3 :: (Monad m, Functor m, ToMRef3 t) => t m a -> m (ConnectionType m (a (ConnectionType m)))
--mrefRaw3 = fmap fromRef . toMRef3



--app base args = mkCons .: App <$> mrefRaw base <*> mapM marg args where
--    marg (ArgRef n a) = Arg n <$> fmap fromRef a


----convert1 :: (Convertible (m a) (n a)) => m a -> n (a :: * -> *)
----convert1 = convert

--var2 :: (ASTBuilder a m, ConvertibleM Expr a) => Name -> NodeCons m a --var2 :: (ASTBuilder Expr m, Convertible (Expr x) a) => Name -> NodeCons m a
--var2 = mkCons . convertM . Var

--cons2 :: (ASTBuilder a m, ConvertibleM Expr a) => Name -> NodeCons m a
--cons2 = mkCons . convertM . Cons

--var :: ASTBuilder Expr m => Name -> NodeCons m Expr
--var = mkCons . Var

--cons :: ASTBuilder Expr m => Name -> NodeCons m Expr
--cons = mkCons . Cons

--ref :: (Monad m, IsMVal t m (NodeCons m a)) => Name -> t -> m (Ref m a)
--ref _ = ref_

--ref_ :: (Monad m, IsMVal t m (NodeCons m a)) => t -> m (Ref m a)
--ref_ cons = toMVal cons >>= runNodeCons

----ref2_ :: (ToMRef3 t, Monad m) => t m a -> m (Ref m a)
--ref2_ = toMRef2

--class IsMVal a m b | a -> m b where
--    toMVal :: a -> m b

--instance                      Monad m => IsMVal (NodeCons m a) m (NodeCons m a) where toMVal = return
--instance                      Monad m => IsMVal (Ref m a)      m (Ref m a)      where toMVal = return
--instance {-# OVERLAPPABLE #-} Monad m => IsMVal (m a)          m a              where toMVal = id

--class Monad m => ToMRef m t where
--    toMRef :: t m a -> m (Ref m a)

--instance Monad m => ToMRef m Ref where
--    toMRef = return

--instance Monad m => ToMRef m NodeCons where
--    toMRef = runNodeCons

--class ToMRef2 t m a | t -> m a where
--    toMRef2 :: t -> m (Ref m a)

--instance                      Monad m => ToMRef2 (NodeCons m a) m a where toMRef2 = runNodeCons
--instance                      Monad m => ToMRef2 (Ref m a)      m a where toMRef2 = return
--instance {-# OVERLAPPABLE #-} Monad m => ToMRef2 (m (Ref m a))  m a where toMRef2 = id


--class ToMRef3 t where
--    toMRef3 :: Monad m => t m a -> m (Ref m a)

--instance ToMRef3 NodeCons where toMRef3 = runNodeCons
--instance ToMRef3 Ref      where toMRef3 = return




--instance MonadNodeGraph c (State (NodeGraph c)) where
--    getNodeGraph = get
--    putNodeGraph = put



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


--g2 :: (ConvertibleM Expr a, Monad m, ASTBuilder a m) => m (Ref m a)
--g2 = do
--    a <- ref2_ $ var2 "a"
--    return a

----g1 :: ASTBuilder Expr m => m ()
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





