
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}


module Main where

import Flowbox.Prelude hiding (simple, empty, Indexable, Simple, cons)
import Data.Repr

--import qualified Luna.Inference.Type as Type
import           Luna.Inference.Type
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

data AST h = ASTExpr (h Expr)
         --deriving (Show)

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

data Ptr i a = Ptr { fromPtr :: i }

newtype Unbounded a = Unbounded a deriving (Show, Functor)





--instance At (Unbounded (Vector a)) where
    --at :: Index m -> IndexedLens' (Index m) m (Maybe (IxValue m))

type instance IxValue (Unbounded a) = IxValue a


unsafeAt :: (UncheckedGetIdx m, UncheckedSetIdx m) => Index m -> Lens' m (ElementOf m)
unsafeAt i = lens (uncheckedGetIdx i) (flip $ uncheckedSetIdx i)

--overKey (Key i) = nodes . at i . mapping hiddenLens
--overKey (Ptr i) = nodes . at i . mapping hiddenLens


--test :: Vector Int -> Vector Int
--test v = v & at 5 .~ 8

--class UncheckedGetIdx a where
--    uncheckedGetIdx :: IndexType a -> a -> ElementOf a

--class UncheckedSetIdx a where
--    uncheckedSetIdx :: (IndexType a) -> (ElementOf a) -> a -> a

--test :: Int -> Iso' a b
--test i = iso (uncheckedGetIdx i) (uncheckedSetIdx i)

--test =

mapOver :: (Lens' b a) -> (a -> (a, x)) -> (b -> (b, x))
mapOver lens f s = (s & lens .~ a, out) where
    (a, out) = f $ s ^. lens



instance (Typeable a, Show i) => Show (Ptr i a) where
    show (Ptr i) = "Ptr \"" <> show (typeOf (undefined :: a)) <> "\" " <> show i



deriving instance Show (HExpr h) => Show (Expr h)

instance Repr (Expr h) where
    repr = \case
        Var      n   -> "Var "      <> show (unpackFS n)
        Cons     n   -> "Cons "     <> show (unpackFS n)
        Accessor n _ -> "Accessor " <> show (unpackFS n)
        App      {}  -> "App"

--data Var = Var Name deriving (Show)

data Arg h = Arg { _label :: Maybe Name, _val :: HExpr h }

deriving instance Show (HExpr h) => Show (Arg h)

makeLenses ''Expr
makeLenses ''Arg



class Node n inp | n -> inp where
    inputs :: n -> [inp]



instance Node (Expr h) (HExpr h) where
    inputs = \case
        Accessor _    base -> [base]
        App      base args -> base : fmap (view val) args
        _                  -> []




--unsafeGet :: NodeGraph m a => NodePtr m a -> Graph m -> a
--unsafeGet ptr = view $ unsafeOverPtr ptr

      --inputs :: Graph -> ID -> [Key Expr]
        --inputs g id = case IntMap.lookup id (g ^. nodes) of
        --    Nothing -> []
        --    Just n  -> exprOutputs (fromNodeObject n)
        --    where exprOutputs :: Expr -> [Key Expr]
        --          exprOutputs = \case
        --              Accessor _ c  -> [c]
        --              App      c as -> c : fmap (view val) as
        --              _             -> []



--class ArgCons a b where
--  arg :: a -> b

--instance ArgCons Name (HExpr h -> Arg h) where
--  arg = Arg . Just

--instance ArgCons (HExpr h) (Arg h) where
--  arg = Arg Nothing


        ----data Node = Node { _tp   :: ID
        ----                 , _expr :: Expr
        ----                 , _out  :: [Key Expr]
        ----                 } deriving (Show)



        ------class IsNode
        ----class Node n where
        ----    inputs  :: n -> [Key Expr]
        ----    outputs :: n -> [Key Expr]




        --type IsExpr a = (Show a, Typeable a, Repr a)


        ----instance Convertible Var Expr where
        ----    convert = VarE


        ----inputs = [0]

        ----g = empty
        ----  & Graph.insNode (0, Node 0 $ Input    "a"  )
        ----  & Graph.insNode (1, Node 1 $ Accessor "foo")
        ----  & Graph.insNode (2, Node 2 $ App)
        ----  & Graph.insNode (3, Node 3 $ Cons "Main")
        ----  & Graph.insNode (4, Node 4 $ Accessor "bar")
        ----  & Graph.insNode (5, Node 5 $ App)


        ----newtype Key t = Key Int


--- === [TOEXTRACT] Instances ===
instance Default (Vector a) where def = mempty


--- === NodeObject ===

type NodeVal a = (Show a, Typeable a, Repr a)

data NodeObject where
    NodeObject :: NodeVal a => a -> NodeObject



hiddenLens :: IsoCastable a b => Iso' a b
hiddenLens = iso cast cast

-- instances

instance Show NodeObject where show (NodeObject a) = show a
instance Repr NodeObject where repr (NodeObject a) = repr a

instance {-# OVERLAPPABLE #-} Castable NodeObject a where
    cast (NodeObject a) = unsafeCoerce a

instance {-# OVERLAPPABLE #-} NodeVal a => Castable a NodeObject where
    cast = NodeObject

--- === Graph ===

data Graph nodes = Graph { _nodes      :: nodes
                         , _argNames   :: [Name]
                         }

makeLenses ''Graph

class HasGraph a n | a -> n where
    graph :: Lens' a (Graph n)


type NodePtr m a = Ptr (Index m) a
type NodeGraph m a = (UncheckedSetIdx m, UncheckedGetIdx m, IsoCastable a (ElementOf m), NodeVal a)

unsafeOverPtr :: NodeGraph m a => NodePtr m a -> Lens' (Graph m) a
unsafeOverPtr (Ptr i) = nodes . unsafeAt i . hiddenLens

unsafeSet :: NodeGraph m a => NodePtr m a -> a -> Graph m -> Graph m
unsafeSet ptr a = unsafeOverPtr ptr .~ a

unsafeGet :: NodeGraph m a => NodePtr m a -> Graph m -> a
unsafeGet ptr = view $ unsafeOverPtr ptr

insert :: (Castable a (ElementOf m), Appendable m, Indexable m)
       => a -> Graph m -> (Graph m, NodePtr m a)
insert a g = (g', Ptr . lastIdx $ g' ^. nodes) where
    g' = g & nodes %~ (flip append (cast a))


-- FIXME (unsafe implementation)
inputs' :: Graph (Vector NodeObject) -> ID -> [Ptr Int (Expr (Ptr Int))]
inputs' g id = inputs $ unsafeGet (Ptr id :: Ptr Int (Expr (Ptr Int))) g

--        ---- instances

deriving instance Show nodes => Show (Graph nodes)

instance Default nodes => Default (Graph nodes) where
    def = Graph def def



--- === GraphBuilder ===

type GraphBuilder c = State (BldrState c)


type family ConnectionType (m :: * -> *) :: * -> *

type instance ConnectionType (GraphBuilder c) = Ptr (Index c)

data BldrState c = BldrState { _nodeScope :: [Int]
                             , __graph     :: Graph c
                             }



makeLenses ''BldrState

instance Default (Graph c) => Default (BldrState c) where
    def = BldrState def def

instance HasGraph (BldrState c) c where
    graph = _graph

class Monad m => MonadBldrState c m | m -> c where
    getBldrState :: m (BldrState c)
    putBldrState :: BldrState c -> m ()


class HasBldrState a c | a -> c where
    bldrState :: Lens' a (BldrState c)

instance HasBldrState (BldrState c) c where
    bldrState = id

-- utils

withBldrState_ :: MonadBldrState c m => (BldrState c -> BldrState c) -> m ()
withBldrState_ f = withBldrState $ fmap (,()) f


withBldrState :: MonadBldrState c m => (BldrState c -> (BldrState c, a)) -> m a
withBldrState f = do
    bldr <- getBldrState
    let (bldr', out) = f bldr
    putBldrState $ bldr'
    return out

withGraph :: MonadBldrState c m => (Graph c -> (Graph c, a)) -> m a
withGraph = withBldrState . mapOver graph

requestNodeID :: MonadBldrState c m => m ID
requestNodeID = do
    s <- getBldrState
    let nsLens     = bldrState . nodeScope
        (id : ids) = s ^. nsLens
    putBldrState $ s & nsLens .~ ids
    return id

releaseNodeID :: MonadBldrState c m => ID -> m ()
releaseNodeID id = withBldrState_ (nodeScope %~ (id:))

--add :: (Appendable c, Indexable c, Castable a (ElementOf c), MonadBldrState c m)
--    => a -> m (Ptr (Index c) a)
--add = withGraph . insert

class Monad m => ASTBuilder a m where
    mkRef :: ASTNode m a -> m (Ref m a)

type ASTNode m a = a (ConnectionType m)
--type Ref m a = m (ConnectionType m (a (ConnectionType m)))
newtype Ref m a = Ref { fromRef :: ConnectionType m (a (ConnectionType m)) }

type GraphBuilderMonad c a = (Appendable c, Indexable c, Castable (a (Ptr (Index c))) (ElementOf c))



instance GraphBuilderMonad c a => ASTBuilder a (GraphBuilder c) where
    mkRef = fmap Ref . withGraph . insert

--add' :: el (ConnectionType bldr) -> bldr
--add' = add

--type X =

--instance (Castable a (ElementOf c), idx ~ Index c, Appendable c, Indexable c)
--      => ASTBuilder a (GraphBuilder idx c (Ptr idx a)) where
--    add' = add

-- instances

--Expr (Ptr Int)

--data Nu f=forall a.Nu a (a->f a)
--newtype Mu f = Mu (f (Mu f))

--a = undefined :: Mu ()



--foo :: State (BldrState (Vector NodeObject)) (Ptr Int (Expr (Ptr Int)))
--foo :: (Appendable c, LastIdx c, SetIdx c, GetIdx c,
--                       CheckedSetIdx c, CheckedGetIdx c, UncheckedSetIdx c,
--                       UncheckedGetIdx c, Castable (Expr h) (ElementOf c),
--                       MonadBldrState c m) =>
--                      m (Ptr (Index c) (Expr h))

newtype NodeCons m a = NodeCons { runNodeCons :: m (Ref m a) }


returnClone :: Monad m => m a -> m (m a)
returnClone = return

refToCons :: Monad m => m (Ref m a) -> m (NodeCons m a)
refToCons = return . NodeCons

--refToCons2 :: m (Ref m a) -> NodeCons m a
--refToCons2 = NodeCons


mkCons = NodeCons . mkRef

--accessor name el = mkRef $ Accessor name el
access = flip accessor

accessor name el = mkCons . Accessor name <$> mrefRaw el

mrefRaw = fmap fromRef . toMRef2



app base args = mkCons .: App <$> mrefRaw base <*> mapM marg args where
    marg (ArgRef n a) = Arg n <$> fmap fromRef a




var :: ASTBuilder Expr m => Name -> NodeCons m Expr
var = mkCons . Var

cons :: ASTBuilder Expr m => Name -> NodeCons m Expr
cons = mkCons . Cons

ref :: (Monad m, IsMVal t m (NodeCons m a)) => Name -> t -> m (Ref m a)
ref _ = ref_

ref_ :: (Monad m, IsMVal t m (NodeCons m a)) => t -> m (Ref m a)
ref_ cons = toMVal cons >>= runNodeCons


class IsMVal a m b | a -> m b where
    toMVal :: a -> m b

instance                      Monad m => IsMVal (NodeCons m a) m (NodeCons m a) where toMVal = return
instance                      Monad m => IsMVal (Ref m a)      m (Ref m a)      where toMVal = return
instance {-# OVERLAPPABLE #-} Monad m => IsMVal (m a)          m a              where toMVal = id

class Monad m => ToMRef m t where
    toMRef :: t m a -> m (Ref m a)

instance Monad m => ToMRef m Ref where
    toMRef = return

instance Monad m => ToMRef m NodeCons where
    toMRef = runNodeCons

class ToMRef2 t m a | t -> m a where
    toMRef2 :: t -> m (Ref m a)

instance                      Monad m => ToMRef2 (NodeCons m a) m a where toMRef2 = runNodeCons
instance                      Monad m => ToMRef2 (Ref m a)      m a where toMRef2 = return
instance {-# OVERLAPPABLE #-} Monad m => ToMRef2 (m (Ref m a))  m a where toMRef2 = id

instance MonadBldrState c (State (BldrState c)) where
    getBldrState = get
    putBldrState = put



runNodeBuilder :: GraphBuilder (Vector NodeObject) a -> Graph (Vector NodeObject)
runNodeBuilder = view graph . flip execState def


--g2 :: Graph (Vector NodeObject)
--g2 = runNodeBuilder $ do
--    a    <- var "a"
--    mod  <- var "Main"
--    foo  <- a    @.  "foo"
--    b    <- foo  @$$ [a]
--    bar  <- mod  @.  "bar"
--    c    <- bar  @$$ [b]
--    plus <- mod  @.  "plus"
--    out  <- plus @$$ [c, a]
--    return ()

--data Arg h = Arg { _label :: Maybe Name, _val :: HExpr h }


--class Arg2 a where
--    arg2 :: a -> b

--instance Arg2 Name -> (a -> Arg a)

class Named a where
    named :: Name -> a -> a

instance Named (ArgRef m a) where
    named n (ArgRef _ ref) = ArgRef (Just n) ref

data ArgRef m a = ArgRef (Maybe Name) (m (Ref m a))

arg = ArgRef Nothing . toMRef2

g1 :: ASTBuilder Expr m => m ()
g1 = do
    a    <- ref "a"    $ var "a"
    mod  <- ref "mod"  $ cons "Mod"
    foo  <- ref "foo"  $ a    @. "foo"
    b    <- ref "b"    $ foo  @$ [arg a]
    bar  <- ref "bar"  $ mod  @. "bar"
    c    <- ref "c"    $ bar  @$ [arg b]
    plus <- ref "plus" $ mod  @. "plus"
    out  <- ref "out"  $ plus @$ [arg c, arg a]
    return ()

--dorobic budowanie liniowe, nie grafowe
--zmienne powinny odnosic sie do siebie jako debrouigle - moze warto parametryzowac Name, tak y mozna bylo wsadzic tam ID debrouiglowe?

(@.)  = access
(@$)  = app

main = do
    let g  = runNodeBuilder g1
        gv = toGraphViz g
    print g
    --print $ toDot gv
    runGraphviz gv Png "/tmp/out.png"
    createProcess $ shell "open /tmp/out.png"
    print "end"


data Label a e = Label a e
type Labeled l a = Label l (a (Label l))
type LExpr l = Labeled l Expr

newtype Simple a = Simple a deriving (Show)
type SExpr = Simple (Expr Simple)

--newtype Mu f = Mu (f (Mu f))


--inputs' :: Graph (Vector NodeObject) -> ID -> [Ptr Int (Expr (Ptr Int))]

toGraphViz :: Graph (Vector NodeObject) -> DotGraph Int
toGraphViz g = DotGraph { strictGraph     = False
                        , directedGraph   = True
                        , graphID         = Nothing
                        , graphStatements = DotStmts { attrStmts = []
                                                     , subGraphs = []
                                                     , nodeStmts = nodeStmts
                                                     , edgeStmts = edgeStmts
                                                     }
                        }
    where ns              = g ^. nodes
          nodeIds         = [0 .. Vector.length ns - 1] :: [Int]
          elems           = fmap ((Vector.!) ns) nodeIds
          nodeLabels      = fmap repr elems
          labeledNode s a = DotNode a [GV.Label . StrLabel $ fromString s]
          nodeStmts       = fmap (uncurry labeledNode) $ zip nodeLabels nodeIds
          nodeInEdges   n = zip3 [0..] (fmap fromPtr $ inputs' g n) (repeat n)
          inEdges         = concat $ fmap nodeInEdges nodeIds
          mkEdge  (n,a,b) = DotEdge a b [GV.Label . StrLabel $ fromString $ show n]
          edgeStmts       = fmap mkEdge inEdges





