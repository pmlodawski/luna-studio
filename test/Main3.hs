
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




--h (Expr h) , h = TExpr => TExpr (Expr TExpr) === Ptr Int (TExpr Type (Expr TExpr))

data AST h = ASTExpr (h Expr)
         --deriving (Show)

data Arg e = Arg { _label :: Maybe Name, _val :: e } deriving (Show)




-- wszystkie rzeczy nazwane powinny byc w slownikach w jezyku! - czyli datatypy ponizej zostaja,
-- ale mozemy tworzyc np. Var funkcjami, ktore oczekuja konkretnego Stringa!
data Expr e = Var      { _name :: Name                               }
            | Cons     { _name :: Name                               }
            | Accessor { _name :: Name, _base :: e                   }
            | App      {                _base :: e, _args :: [Arg e] }
            deriving (Show)


makeLenses ''Expr
makeLenses ''Arg




data Typex = Typex -- fixme

newtype Mu f = Mu { _mu :: f (Mu f) }

data    TExpr     e = TExpr Typex (Expr e)
data    Label l m e = Label l (m e)

newtype Ptr   i a   = Ptr  { fromPtr :: i           }
newtype MPtr  i m e = MPtr { _mptr   :: Ptr i (m e) }

--type    MuPtr    i m = Mu (MPtr  i m)
type    MuPtr    i m = Mu (MPtr  i m)
type    MuLab    l m = Mu (Label l m)

type    MuPtrExpr  i = MuPtr i Expr
type    MuPtrTExpr i = MuPtr i TExpr
type    MuLabExpr  l = MuLab l Expr
type    MuExpr       = Mu Expr


deriving instance Show (f (Mu f)) => Show (Mu f)
deriving instance (Show i, Typeable m, Typeable e) => Show (MPtr i m e)
--deriving instance (Show i, Typeable m, Typeable i) => Show (MuPtr i m)


instance ConvertibleM Expr Expr where convertM = id

makeLenses ''Mu
makeLenses ''MPtr

class HasPtr p i a | p -> i a where
    ptr :: Lens' p (Ptr i a)

instance                          HasPtr (Ptr i a)    i a     where ptr = id
instance                          HasPtr (MPtr i m e) i (m e) where ptr = mptr
instance HasPtr (m (Mu m)) i a => HasPtr (Mu m)       i a     where ptr = mu . ptr






unsafeAt :: (UncheckedGetIdx m, UncheckedSetIdx m) => Index m -> Lens' m (ElementOf m)
unsafeAt i = lens (uncheckedGetIdx i) (flip $ uncheckedSetIdx i)

mapOver :: (Lens' b a) -> (a -> (a, x)) -> (b -> (b, x))
mapOver lens f s = (s & lens .~ a, out) where
    (a, out) = f $ s ^. lens


instance (Typeable a, Show i) => Show (Ptr i a) where
    show (Ptr i) = "Ptr (" <> show (typeOf (undefined :: a)) <> ") " <> show i


instance Repr (Expr e) where
    repr = \case
        Var      n   -> "Var "      <> show (unpackFS n)
        Cons     n   -> "Cons "     <> show (unpackFS n)
        Accessor n _ -> "Accessor " <> show (unpackFS n)
        App      {}  -> "App"


    --class IsNode n inp | n -> inp where
    --    inputs :: n -> [inp]



    --instance IsNode (Expr h) (HExpr h) where
    --    inputs = \case
    --        Accessor _    base -> [base]
    --        App      base args -> base : fmap (view val) args
    --        _                  -> []





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


type GraphPtr m a = Ptr (Index m) a
type GraphCtx m a = (UncheckedSetIdx m, UncheckedGetIdx m, IsoCastable a (ElementOf m), NodeVal a)

unsafeOverPtr :: GraphCtx m a => GraphPtr m a -> Lens' (Graph m) a
unsafeOverPtr (Ptr i) = nodes . unsafeAt i . hiddenLens

unsafeSet :: GraphCtx m a => GraphPtr m a -> a -> Graph m -> Graph m
unsafeSet ptr a = unsafeOverPtr ptr .~ a

unsafeGet :: GraphCtx m a => GraphPtr m a -> Graph m -> a
unsafeGet ptr = view $ unsafeOverPtr ptr

insert :: (Castable a (ElementOf m), Appendable m, Indexable m)
       => a -> Graph m -> (Graph m, GraphPtr m a)
insert a g = (g', Ptr . lastIdx $ g' ^. nodes) where
    g' = g & nodes %~ (flip append (cast a))


---- FIXME (unsafe implementation)
--inputs' :: Graph (Vector Hidden) -> ID -> [Ptr Int (Expr (Ptr Int))]
--inputs' g id = inputs $ unsafeGet (Ptr id :: Ptr Int (Expr (Ptr Int))) g

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



----- === Builder ===


data NodeGraph c = NodeGraph { _nodeScope :: [Int]
                             , __graph    :: Graph c
                             }



type Builder c = State (NodeGraph c)

type family Element        (m :: * -> *) (a :: (* -> *) -> *) :: (* -> *) -> *
type family ConnectionType (m :: * -> *) :: * -> *
--type family ConnectionType2 (m :: * -> *) :: (* -> *) -> *
type family ASTNode2       (m :: * -> *) (a :: (* -> *) -> *) :: *

--type family ConnectionType2 (m :: * -> *) (a :: *) :: *

type family Connection (tp :: * -> *) (a :: (* -> *) -> *)

--type instance Connection tp Expr = tp (Expr tp)

----type instance Connection tp Expr = tp (Expr tp)


makeLenses ''NodeGraph

instance MonadNodeGraph c (Builder c) where
    getNodeGraph = get
    putNodeGraph = put


type instance ConnectionType (Builder c) = Ptr (Index c)
--type instance ConnectionType2 (Builder c) = MuPtr (Index c)
type instance Element        (Builder c) a = a
type instance ASTNode2       (Builder c) a = a (ConnectionType (Builder c))




instance Default (Graph c) => Default (NodeGraph c) where
    def = NodeGraph def def

instance HasGraph (NodeGraph c) c where
    graph = _graph

class Monad m => MonadNodeGraph c m | m -> c where
    getNodeGraph :: m (NodeGraph c)
    putNodeGraph :: NodeGraph c -> m ()


class HasNodeGraph a c | a -> c where
    nodeGraph :: Lens' a (NodeGraph c)

instance HasNodeGraph (NodeGraph c) c where
    nodeGraph = id

---- utils

withNodeGraph_ :: MonadNodeGraph c m => (NodeGraph c -> NodeGraph c) -> m ()
withNodeGraph_ f = withNodeGraph $ fmap (,()) f


withNodeGraph :: MonadNodeGraph c m => (NodeGraph c -> (NodeGraph c, a)) -> m a
withNodeGraph f = do
    bldr <- getNodeGraph
    let (bldr', out) = f bldr
    putNodeGraph $ bldr'
    return out

withGraph :: MonadNodeGraph c m => (Graph c -> (Graph c, a)) -> m a
withGraph = withNodeGraph . mapOver graph

requestNodeID :: MonadNodeGraph c m => m ID
requestNodeID = do
    s <- getNodeGraph
    let nsLens     = nodeGraph . nodeScope
        (id : ids) = s ^. nsLens
    putNodeGraph $ s & nsLens .~ ids
    return id

releaseNodeID :: MonadNodeGraph c m => ID -> m ()
releaseNodeID id = withNodeGraph_ (nodeScope %~ (id:))

----add :: (Appendable c, Indexable c, Castable a (ElementOf c), MonadNodeGraph c m)
----    => a -> m (Ptr (Index c) a)
----add = withGraph . insert



-- === ASTBuilder ===


newtype Ref r a = Ref { fromRef :: r a }

newtype Ref2 i m = Ref2 { fromRef2 :: MuPtr i m }

--newtype Ref2 a i = Ref2 { fromRef2 :: MuPtr a i }

class ASTBuilder a m where
    mkRef :: a -> m (Ref (ConnectionType m) a)

class ASTBuilder2 a m where
    mkRef2 :: a -> m (ConnectionType m a)

instance (Appendable c, Indexable c, Castable a (ElementOf c)) => ASTBuilder a (Builder c) where
    mkRef = fmap Ref . withGraph . insert

instance (Appendable c, Indexable c, Castable a (ElementOf c)) => ASTBuilder2 a (Builder c) where
    mkRef2 = withGraph . insert

--instance (Appendable c, Indexable c, Castable a (ElementOf c)) => ASTBuilder2 v a (Builder c) where
--mkRef2' = fmap (MuPtr . Mu . MPtr) . withGraph . insert

xfoo = Ref2 . Mu . MPtr

xtest :: (Functor m, ASTBuilder2 (a (MuPtr i a)) m, ConnectionType m ~ Ptr i)
      => a (MuPtr i a) -> m (Ref2 i a)
xtest = fmap xfoo . mkRef2

newtype NodeCons m a = NodeCons { runNodeCons :: m (Ref (ConnectionType m) a) }
--newtype NodeCons3 m a = NodeCons3 { runNodeCons3 :: m (Ref3 m a) }


--returnClone :: Monad m => m a -> m (m a)
--returnClone = return

--refToCons :: Monad m => m (Ref m a) -> m (NodeCons m a)
--refToCons = return . NodeCons

----refToCons2 :: m (Ref m a) -> NodeCons m a
----refToCons2 = NodeCons

mkCons :: ASTBuilder a m => a -> NodeCons m a
mkCons = NodeCons . mkRef

--mkCons2 = NodeCons . xtest
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
--var :: ASTBuilder (Expr e) m => Name -> NodeCons m (Expr e)
var :: (ASTBuilder exp m, ConvertibleM Expr v, exp ~ v e) => Name -> NodeCons m exp
var = mkCons . convertM . Var




runNodeBuilder :: Builder (Vector Hidden) a -> Graph (Vector Hidden)
runNodeBuilder = view graph . flip execState def

runNodeBuilder2 :: Builder (Vector Hidden) a -> a
runNodeBuilder2 = flip evalState def


v = var "foo"
a :: MuPtrExpr Int
a = Mu $ MPtr $ fromRef $ runNodeBuilder2 $ runNodeCons v

main = do
    print a
    print "end"


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





