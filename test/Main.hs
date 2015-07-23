
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}


module Main where

import Flowbox.Prelude hiding (simple, empty, Indexable)
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
import Data.Typeable
import Control.Monad.State

import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing (toDot)
import Data.GraphViz.Commands

import Data.Vector hiding ((++), splitAt, length, convert)
import qualified Data.Vector as Vector
import Data.Vector.Mutable hiding (splitAt, length, convert)
import Data.Maybe (fromJust)

import Data.Indexable

import Data.Containers

add :: Int -> Int -> Int
add = (+)

add2 :: (Int, (Int, ())) -> Int
add2 (a, (b, ())) = a + b


testf :: (Int, (String, ())) -> (Int,String)
testf (a,(b,())) = (a,b)

--unpackTest :: Object -> (Int,String)
--unpackTest = unpackRawData


--unpackInt :: Object -> Int
--unpackInt = unpackRawData

f = unsafeCoerce add :: Any

appx :: a -> Any -> Any
appx a f = (unsafeCoerce f :: Any -> Any) (unsafeCoerce a :: Any)

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

data Key t = Key { fromKey :: ID } deriving (Show) -- { overKey :: Lens' Graph (Maybe t) }

--makeLenses ''Key


type Name = FastString


type HExpr h = h (Expr h)

data AST h = ASTExpr (h Expr)
         --deriving (Show)

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

data Ptr i a = Ptr i

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


instance (Typeable a, Show i) => Show (Ptr i a) where
    show (Ptr i) = "Ptr \"" <> show (typeOf (undefined :: a)) <> "\" " <> show i



deriving instance Show (HExpr h) => Show (Expr h)

        --instance Repr Expr where
        --    repr = \case
        --        Var      n   -> "Var "      <> show (unpackFS n)
        --        Cons     n   -> "Cons "     <> show (unpackFS n)
        --        Accessor n _ -> "Accessor " <> show (unpackFS n)
        --        App      {}  -> "App"

        ----data Var = Var Name deriving (Show)

data Arg h = Arg { _label :: Maybe Name, _val :: HExpr h }

deriving instance Show (HExpr h) => Show (Arg h)

makeLenses ''Expr
makeLenses ''Arg


        --class ArgCons a b | a -> b where
        --  arg :: a -> b

        --instance ArgCons Name (Key Expr -> Arg) where
        --  arg = Arg . Just

        --instance ArgCons (Key Expr) Arg where
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

--- === [TOEXTRACT] GraphClass ===

-- TODO: GraphClass -> Graph w module
class GraphClass g where
    type Node g


--- === NodeObject ===

type NodeVal a = (Show a, Typeable a, Repr a)

data NodeObject where
    NodeObject :: NodeVal a => a -> NodeObject


fromNodeObject :: NodeObject -> a
fromNodeObject (NodeObject a) = unsafeCoerce a

hiddenLens :: NodeVal a => Iso' NodeObject a
hiddenLens = iso fromNodeObject NodeObject

-- instances

instance Show NodeObject where show (NodeObject a) = show a
instance Repr NodeObject where repr (NodeObject a) = repr a


--- === Graph ===

data Graph cont = Graph { _nodes      :: cont NodeObject
                        , _argNames   :: [Name]
                        }

makeLenses ''Graph

type NodePtr m a = Ptr (Index (m NodeObject)) a
type NodeGraph m = (ElementOf (m NodeObject) ~ NodeObject, UncheckedSetIdx (m NodeObject), UncheckedGetIdx (m NodeObject))

unsafeOverPtr :: (NodeVal a, NodeGraph m)
              => NodePtr m a -> Lens' (Graph m) a
unsafeOverPtr (Ptr i) = nodes . unsafeAt i . hiddenLens

unsafeSet :: (NodeVal a, NodeGraph m) => NodePtr m a -> a -> Graph m -> Graph m
unsafeSet ptr a = unsafeOverPtr ptr .~ a

unsafeGet :: (NodeVal a, NodeGraph m) => NodePtr m a -> Graph m -> a
unsafeGet ptr = view $ unsafeOverPtr ptr



insert :: (Convertible a NodeObject, Appendable' m, Indexable (m (NodeObject)), ElementOf (m NodeObject) ~ NodeObject)
       => a -> Graph m -> (Graph m, NodePtr m a)
insert a g = (g', Ptr . lastIdx $ g' ^. nodes) where
    g' = g & nodes %~ (flip append' (convert a))

--        ---- instances

deriving instance Show (m NodeObject) => Show (Graph m)

instance Default (m NodeObject) => Default (Graph m) where
    def = Graph def def



--- === GraphBuilder ===

data GraphBuilder c = GraphBuilder { _nodeScope :: [Int]
                                   , _graph     :: Graph c
                                   }

makeLenses ''GraphBuilder

type MonadGraphBuilder c s m = (MonadState s m, HasGraphBuilder s c)

class HasGraphBuilder a c | a -> c where
    graphBuilder :: Simple Lens a (GraphBuilder c)

-- utils

withGraphBuilder :: MonadGraphBuilder c s m => (GraphBuilder c -> GraphBuilder c) -> m ()
withGraphBuilder f = do
    s <- get
    put $ s & graphBuilder %~ f

requestNodeID :: MonadGraphBuilder c s m => m ID
requestNodeID = do
    s <- get
    let nsLens     = graphBuilder . nodeScope
        (id : ids) = s ^. nsLens
    put $ s & nsLens .~ ids
    return id

releaseNodeID :: MonadGraphBuilder c s m => ID -> m ()
releaseNodeID id = withGraphBuilder (nodeScope %~ (id:))

-- instances

instance Default (Graph c) => Default (GraphBuilder c) where
    def = GraphBuilder def def


--registerNode :: (MonadGraphBuilder m, IsExpr a) => Key a -> a -> m ()
--registerNode k a = withBuilder $ graph %~ insert k a

--registerInput :: MonadGraphBuilder m => Name -> m ()
--registerInput name = withBuilder $ graph . argNames %~ (<> [name])

--mkNode :: (MonadGraphBuilder m, IsExpr a) => a -> m (Key a)
--mkNode a = do
--    key <- newNodeKey
--    registerNode key a
--    return key

-------------------------------

--g :: Graph (Vector NodeObject)
--g = def

main = do
    --print g
    let v = Vector.fromList $ [1,2,3] :: Vector Int
        --v2 = Vector.modify (\v -> write v 3 0) v
    --print v2
    print "end"





        --inputs :: Graph -> ID -> [Key Expr]
        --inputs g id = case IntMap.lookup id (g ^. nodes) of
        --    Nothing -> []
        --    Just n  -> exprOutputs (fromNodeObject n)
        --    where exprOutputs :: Expr -> [Key Expr]
        --          exprOutputs = \case
        --              Accessor _ c  -> [c]
        --              App      c as -> c : fmap (view val) as
        --              _             -> []


        ----keyBy


        --unsafeKeyByID :: ID -> Key a
        --unsafeKeyByID = Key

        ----unsafeKeyByID :: IsExpr a => Int -> Key a
        ----unsafeKeyByID i = Key $ nodes . at i . mapping hiddenLens


        --overKey (Key i) = nodes . at i . mapping hiddenLens

        --hiddenLens :: IsExpr a => Iso' NodeObject a
        --hiddenLens = iso fromNodeObject NodeObject


        --fromNodeObject :: NodeObject -> a
        --fromNodeObject (NodeObject a) = unsafeCoerce a

        ----unhideExpr :: NodeObject -> Expr
        ----unhideExpr (NodeObject a) = convert a


        --insert :: IsExpr a => Key a -> a -> Graph -> Graph
        --insert key a = overKey key .~ (Just a)


        --insertAt :: IsExpr a => Int -> a -> Graph -> Graph
        --insertAt = insert . unsafeKeyByID

        --lookup :: IsExpr a => Key a -> Graph -> Maybe a
        --lookup = view . overKey

        ----lookupAt :: Int -> Graph -> Maybe Expr
        ----lookupAt i g = unhideExpr <$> IntMap.lookup i (view nodes g)

        ----lookupAt2 :: Int -> Graph -> Maybe Expr
        ----lookupAt2 i g = unhideExpr <$> IntMap.lookup i (view nodes g)

        --convertIso :: IsoConvertible a b => Iso' a b
        --convertIso = iso convert convert

        ----instance {-# OVERLAPPABLE #-} IsoConvertible a b => Convertible (Key a) (Key b) where convert (Key lens) = Key $ lens . convertIso



        --type MonadGraphBuilder m = MonadState GraphBuilder m

        --data GraphBuilder = GraphBuilder { _lastKey :: Int
        --                                 , _lastNode       :: Int
        --                                 , _graph          :: Graph
        --                                 }

        --instance Default GraphBuilder where
        --    def = GraphBuilder def def def



        --makeLenses ''GraphBuilder

        --getBuilder :: MonadGraphBuilder m => m GraphBuilder
        --getBuilder = get

        --putBuilder :: MonadGraphBuilder m => GraphBuilder -> m ()
        --putBuilder = put

        --newKey :: (IsExpr a, MonadGraphBuilder m) => m (Key a)
        --newKey = do
        --    bldr <- getBuilder
        --    let i = bldr ^. lastKey
        --    put $ bldr & lastKey .~ (i + 1)
        --    return $ unsafeKeyByID i


        --newNodeKey :: (IsExpr a, MonadGraphBuilder m) => m (Key a)
        --newNodeKey = do
        --    bldr <- getBuilder
        --    let i = bldr ^. lastNode
        --    put $ bldr & lastNode .~ (i + 1)
        --    return $ unsafeKeyByID i

        --registerNode :: (MonadGraphBuilder m, IsExpr a) => Key a -> a -> m ()
        --registerNode k a = withBuilder $ graph %~ insert k a

        --registerInput :: MonadGraphBuilder m => Name -> m ()
        --registerInput name = withBuilder $ graph . argNames %~ (<> [name])

        --mkNode :: (MonadGraphBuilder m, IsExpr a) => a -> m (Key a)
        --mkNode a = do
        --    key <- newNodeKey
        --    registerNode key a
        --    return key



        --withBuilder :: MonadGraphBuilder m => (GraphBuilder -> GraphBuilder) -> m ()
        --withBuilder f = do
        --    bldr <- getBuilder
        --    putBuilder $ f bldr

        ----input :: MonadGraphBuilder m => Name -> m (Key Expr)

        --var :: MonadGraphBuilder m => Name -> m (Key Expr)
        --var n = mkNode $ Var n

        --input :: MonadGraphBuilder m => Name -> m (Key Expr)
        --input n = do registerInput n
        --             var n

        ----var :: MonadGraphBuilder m => Name -> m (Key Expr)
        ----var n = mkNode $ VarE $ Var n


        --cons :: MonadGraphBuilder m => Name -> m (Key Expr)
        --cons n = mkNode $ Cons n


        --class WithKeyLike m a where
        --    withKeyLike :: a -> (Key Expr -> m (Key Expr)) -> (m (Key Expr))

        --instance WithKeyLike m (Key Expr) where
        --    withKeyLike key f = f key

        --instance (m ~ n, Monad m) => WithKeyLike m (n (Key Expr)) where
        --    withKeyLike key f = flip withKeyLike f =<< key



        ----access :: (PolyFunc1 (Key Expr) m a, MonadGraphBuilder m) => a -> Name -> m (Key Expr)
        ----access key name = polyFunc1 (mkNode . Accessor name) key

        ----app ::


        --access :: MonadGraphBuilder m => Name -> Key Expr -> m (Key Expr)
        --access name key = mkNode $ Accessor name key

        --app :: MonadGraphBuilder m => Key Expr -> [Arg] -> m (Key Expr)
        --app base args = mkNode $ App base args

        --appArgs :: MonadGraphBuilder m => Key Expr -> [Key Expr] -> m (Key Expr)
        --appArgs base = app base . fmap arg

        --g1 :: Graph
        --g1 = view graph $ flip execState def $ do
        --    a    <- input "a"
        --    foo  <- access "foo" a
        --    b    <- foo `appArgs` [a]
        --    mod  <- var "Main"
        --    bar  <- access "bar" mod
        --    c    <- bar `appArgs` [b]
        --    plus <- access "plus" mod
        --    out  <- plus `appArgs` [c, a]
        --    return ()


        --main = do
        --    --let a = unsafeCoerce (return (1:: Int)) :: Monad m => m Int
        --        --b = unsafeCoerce a :: [Int]
        --        --a = mkObject (1 :: Int)
        --        --b = mkObject (2 :: Int)
        --        --s = mkObject ("ala" :: String)
        --        --c = appSimple add_Int [a,b]
        --        --c = appSimple (toDataFunc add2) [a,b]
        --        --x = appSimple (toDataFunc testf) [a,s]


        --    --print $ unpackInt a
        --    --print $ unpackInt b
        --    --print $ unpackInt c
        --    --print $ unpackTest x
        --    --let ss = fmap (fromString . show) [1..10000] :: [FastString]
        --    --    a  = (==) <$> ss <*> ss
        --    --    b  = length $ filter (==True) a

        --    --print $ b

        --    print $ (unsafeCoerce $ appx (1 :: Int) $ appx (2 :: Int) f :: Int)
        --    print "end"
        --    print g1
        --    print "=========="
        --    print $ toDot (toGraphViz g1)
        --    runGraphviz (toGraphViz g1) Png "/tmp/out.png"
        --    --print graph
        --    --print b
        --    --print g


        --toGraphViz :: Graph -> DotGraph Int
        --toGraphViz g = DotGraph { strictGraph     = False
        --                        , directedGraph   = True
        --                        , graphID         = Nothing
        --                        , graphStatements = DotStmts { attrStmts = []
        --                                                     , subGraphs = []
        --                                                     , nodeStmts = nodeStmts
        --                                                     , edgeStmts = edgeStmts
        --                                                     }
        --                        }
        --    where elems           = IntMap.assocs $ g ^. nodes
        --          nodeIds         = fmap fst elems
        --          nodeLabels      = fmap (repr . snd) elems
        --          labeledNode s a = DotNode a [Label . StrLabel $ fromString s]
        --          nodeStmts       = fmap (uncurry labeledNode) $ zip nodeLabels nodeIds
        --          nodeInEdges   n = zip3 [0..] (fmap fromKey $ inputs g n) (repeat n)
        --          inEdges         = concat $ fmap nodeInEdges nodeIds
        --          mkEdge  (n,a,b) = DotEdge a b [Label . StrLabel $ fromString $ show n]
        --          edgeStmts       = fmap mkEdge inEdges

