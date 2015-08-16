
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Flowbox.Prelude hiding (simple, empty, Indexable, Simple, cons, lookup, index, Wrapped, children, Cons)
import Data.Repr

--import qualified Luna.Inference.Type as Type
--import           Luna.Inference.Type
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Text.Lazy      as Text
import           Data.Text.Lazy      (Text)

--import           Luna.Inference.RawData



import           GHC.Prim (Any)
import           GHC.Int
import           Unsafe.Coerce (unsafeCoerce)
import           Data.Convert

--import qualified Data.Graph.Inductive as Graph
--import           FastString (FastString, mkFastString, unpackFS)


import qualified Data.IntMap.Lazy as IntMap
import           Data.IntMap.Lazy (IntMap)
import Data.Typeable       hiding (cast)
import qualified Control.Monad.State as State
--import Control.Monad.State hiding (withState, mapM)


import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete hiding (Label, Int)
import qualified Data.GraphViz.Attributes.Complete as GV
import Data.GraphViz.Printing (toDot)
import Data.GraphViz.Commands

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable ()
import Data.Maybe (fromJust)

--import Data.Indexable

import Data.Containers
import Data.Containers.Hetero

import System.Process

import qualified Data.Text.AutoBuilder as Text

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Constraint
import Control.Error.Util (hush)
import Data.Convert.Errors (TypeMismatch (TypeMismatch))
import Data.Constraint.Void
--import Data.Variant
import Data.Variant
import qualified Data.Variant as V
import Flowbox.System.Types hiding ((.:))
import           Control.Monad.State.Generate (newState)

import Text.Read (readMaybe)

import           Luna.Syntax.Graph.Builder
import           Luna.Syntax.Graph
import           Luna.Syntax.Term
import           Luna.Syntax.Lit
import           Luna.Syntax.AST
import           Luna.Syntax.Decl

import           Luna.Syntax.Name.Pool

type ID = Int


--- === Foo ===

--data Foo a = Pure
--           | IO       (Maybe a)
--           | Unknown  (Maybe a)
--           | Foo Name (Maybe a)
--           deriving (Show)

data Base = Pure
          | IO
          deriving (Show)

--data Foo = Foo Name deriving (Show)

--data Foo = KnownFoo Base [Name]
--         | UnknownFoo
--         deriving (Show)


-- === Label ===

data Label l a = Label { _lab :: l, _el :: a } deriving (Show)

class LabBuilder m l where
    mkLabel :: m l

makeLenses ''Label

-- instances

instance HasAST a ast => HasAST (Label l a) ast where
    ast = el . ast

instance {-# OVERLAPPABLE #-} (Monad m, Default a) => LabBuilder m a where
    mkLabel = return def

instance {-# OVERLAPPABLE #-} (Applicative m, LabBuilder m l, ASTGen ast m el) => ASTGen ast m (Label l el) where
    genAST a = Label <$> mkLabel <*> genAST a




-- === Function ===

--
data Foo = Foo deriving (Show)
instance Default Foo where def = Foo
--



type FunctionGraph = Function (HomoNet (Label Foo) Term)

-----------------

--data Strictness = Lazy
--                | Strict
--                deriving (Show)
-- Thunk

data Typed a = Typed { _typedData :: a
                     } deriving (Show)

makeLenses ''Typed

instance HasContainer a c => HasContainer (Typed a) c where
    container = typedData . container

instance HasAST a ast => HasAST (Typed a) ast where
    ast = typedData . ast

instance {-# OVERLAPPABLE #-} (ASTGen ast m a, Functor m) => ASTGen ast m (Typed a) where
    genAST a = Typed <$> genAST a

-- ===================================================================


f :: FunctionGraph
f = flip execFunctionBuilder def $ do
    a <- var "a"
    x <- var "x" @. "foo"
    y <- x @$ [arg a]

    return ()

f' = flip execFunctionBuilder (rebuild f) $ do
    b <- var "b"
    return ()




--x2 :: _ => m (GraphRef l Term)
x2 = app (int 5) [arg $ int 6]

--x3 :: (Constructor Lit ast, GraphRefBuilder el m Foo Term, ASTGen ast m el, m ~ (GraphBuilderT (HomoNet Foo Term) Identity))
--   => GraphBuilderT (HomoNet Foo Term) Identity) (GraphRef Foo Term)
--x3 :: (Constructor Lit ast, ASTGen ast m el, RefBuilder el m (Ref (GraphPtr (Label Foo)) Term)) => m (GraphRef (Label Foo) Term)
--x3 = int 3

----x4 :: GraphBuilderT (HomoNet (Label Foo) Term) Identity (GraphRef (Label Foo) Term)
--x4 = int 3


newtype Nest m n a = Nest { _nest :: m (n a) } deriving (Show)

makeLenses ''Nest

type family Nested (wrappers :: [* -> *]) where
    Nested '[m]      = m
    Nested (m ': ms) = Nest m (Nested ms)

--Nest m n a
--Nest m (Nest n l) a

type GraphRef' nest   = GraphRef (Nested nest)
type HomoNet'  nest a = HomoNet  (Nested nest) a

instance Repr (m (n a)) => Repr (Nest m n a) where
    repr = repr . view nest

--TODO[wd]: template haskellize
--instance Wrapper (Nest m n a) where
--    content = lens (\(Nest a) -> a) Nest


instance {-# OVERLAPPABLE #-} (Functor m, ASTGen ast m (t (k a))) => ASTGen ast m (Nest t k a) where
    genAST a = Nest <$> genAST a

instance HasAST (t (k a)) ast => HasAST (Nest t k a) ast where
    ast = nest . ast

--y :: (GraphRef' '[Label Int, Typed] Term, HomoNet' '[Label Int, Typed] Term)
y :: (GraphRef' '[Label Int] Term, HomoNet' '[Label Int] Term)
y = runGraph x2 def





--data TT a = TT a a

--requestM =

--class Releasable p m where
--    release :: ElementOf p

--class Pool p where
--    allocate :: [ElementOf p] -> p

--y3 :: (GraphRef (Label Foo) Term, HomoNet (Label Foo) Term)
--y3 = runGraph x4 def

--(a,b) = y

--c = elems b



-- TODO: poprawic Repr aby byl jak show i nie byl opcjonalny

--preffix   = ""
--suffix    = "#"
--chars     = ['a' .. 'z']
--bases     = return <$> chars
--permute a = fmap (:) chars <*> a
--names     = drop 1 $ fmap (preffix <>) $ concat $ iterate permute [suffix]


main = do
    putStrLn $ repr y
    --print $ take 1000 names
    return ()




--instance ReadColumn

--readColumn :: Proxy t -> [String] -> Maybe (Vector t)



--instance ReadColumn bts '[] where
--    readColumn _ _ _ = Nothing


--instance (Read t, Foo bts ts) => ReadColumn bts (t ': ts) where
--    foo _ _ (a:as) = case (readMaybe a :: Maybe t) of
--        Just val -> Just (mkDataCell val :: DataCol bts)
--        Nothing  -> foo (Proxy :: Proxy bts) (Proxy :: Proxy ts) a





--main = do
--    print f
--    print $ elems f
--    let gv = toGraphViz (view body f)
--    runGraphviz gv Png "/tmp/out.png"
--    createProcess $ shell "open /tmp/out.png"
--    --let m = fck & view body
--    --    g = fck & view fgraph
--    --    Just (Ref p1) = Map.index "foo" m
--    --    r = g ^. reg
--    --    e = unsafeGet p1 r

--    --print fck
--    --print e
--    print "end"


--    --c = mempty :: Hetero' Vector

--    --(c', p') = append ('a') c -- :: (Hetero' Vector, Ptr Int Char)
--    --c'' = prepend_ 'o' c'

--    --main = do
--    --    print "end"
--    --    print (c', p')
--    --    print c''
--    --    print $ uncheckedIndex (Ptr 0 :: Ptr Int Int) c'

----main = do
----    let g  = runNodeBuilder g1
----        gv = toGraphViz g
----    print g
----    --print $ toDot gv
----    runGraphviz gv Png "/tmp/out.png"
----    createProcess $ shell "open /tmp/out.png"
----    print "end"

--toGraphViz :: HomoGraph (HomoNet Foo Term) -> DotGraph Int
--toGraphViz g = DotGraph { strictGraph     = False
--                        , directedGraph   = True
--                        , graphID         = Nothing
--                        , graphStatements = DotStmts { attrStmts = []
--                                                     , subGraphs = []
--                                                     , nodeStmts = nodeStmts
--                                                     , edgeStmts = edgeStmts
--                                                     }
--                        }
--    where nodes           = elems g
--          nodeIds         = indexes g
--          nodeLabels      = fmap repr nodes
--          labeledNode s a = DotNode a [GV.Label . StrLabel $ fromString s]
--          nodeStmts       = fmap (uncurry labeledNode) $ zip nodeLabels nodeIds
--          nodeInEdges   n = zip3 [0..] (fmap ptrIdx . children . ast $ unsafeIndex n g) (repeat n)
--          inEdges         = concat $ fmap nodeInEdges nodeIds
--          mkEdge  (n,a,b) = DotEdge a b [GV.Label . StrLabel $ fromString $ show n]
--          edgeStmts       = fmap mkEdge inEdges











----g1 :: RecBuilder Term m => m ()
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


--class ToMRef t where
--    toMRef :: Monad m => t m a -> m (Ref m a)

--instance ToMRef RefCons where toMRef = runRefCons
--instance ToMRef Ref      where toMRef = return




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

----data Arg h = Arg { _label :: Maybe Name, _val :: Term h }


----class Arg2 a where
----    arg2 :: a -> b

----instance Arg2 Name -> (a -> Arg a)

--class Named a where
--    named :: Name -> a -> a

--instance Named (ArgRef m a) where
--    named n (ArgRef _ ref) = ArgRef (Just n) ref

--data ArgRef m a = ArgRef (Maybe Name) (m (Ref m a))

--arg = ArgRef Nothing . toMRef

----data Node = Node


--g2 :: (ConvertibleM Term a, Monad m, RecBuilder a m) => m (Ref m a)
--g2 = do
--    a <- ref_ $ var "a"
--    return a

----g1 :: RecBuilder Term m => m ()
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
--type LTerm l = Labeled l Term

--newtype Simple a = Simple a deriving (Show)
--type STerm = Simple (Term Simple)

----newtype Mu f = Mu (f (Mu f))


----inputs' :: Graph (Vector Hidden) -> ID -> [Ptr Int (Term (Ptr Int))]

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





