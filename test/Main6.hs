
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
{-# LANGUAGE RecursiveDo #-}

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
import Data.Variants
import qualified Data.Variants as V
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

import Control.Monad.Fix
import Data.Cata

import           Luna.Syntax.Graph.Builder.Star (MonadStarBuilder)
import           Luna.Syntax.Graph.Builder.Star (StarBuilder, StarBuilderT)
import qualified Luna.Syntax.Graph.Builder.Star as StarBuilder

import Luna.Syntax.Graph.Builder.State (GraphBuilder, GraphBuilderT, BldrState, runGraphT) -- TODO[wd] : refactor GraphBuiderT etc out of this module
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



makeLenses ''Label

-- instances

instance HasAST a ast => HasAST (Label l a) ast where
    ast = el . ast



instance {-# OVERLAPPABLE #-} (Applicative m, LabBuilder m l, ASTGen ast m el) => ASTGen ast m (Label l el) where
    genAST a = Label <$> mkLabel <*> genAST a


-- === Labeled ===

data Labeled l a t = Labeled l (a t)
type instance ASTOf (Labeled l a) = ASTOf a


class LabBuilder m l where
    mkLabel :: m l

class HasLabel l a | a -> l where
    label :: Lens' a l

type instance Variants (Labeled l a t) = Variants (a t)
--instance Record (Labeled )
-- instances

deriving instance (Show l, Show (a t)) => Show (Labeled l a t)

instance HasLabel l (Labeled l a t) where
    label = lens (\(Labeled l _) -> l) (\(Labeled _ a) l -> Labeled l a)

instance {-# OVERLAPPABLE #-} (Monad m, Default a) => LabBuilder m a where
    mkLabel = return def

instance HasAST (a t) ast => HasAST (Labeled l a t) ast where ast = undefined
instance HasAST' a ast => HasAST' (Labeled l a) ast where ast' = undefined


instance (ASTGen ast m (a t), LabBuilder m l, Applicative m) => ASTGen ast m (Labeled l a t) where
    genAST a = Labeled <$> mkLabel <*> genAST a





instance (LayerGen t m b, LabBuilder m l) => LayerGen t m (Labeled l b) where
    genLayers a = Labeled <$> mkLabel <*> genLayers a




-- === Typed ===

data Typed a t = Typed t (a t)
type instance ASTOf (Typed a) = ASTOf a

deriving instance (Show t, Show (a t)) => Show (Typed a t)

instance (ASTGen ast m (a t), Applicative m) => ASTGen ast m (Typed a t) where
    genAST a = Typed undefined <$> genAST a
    -- tu zrobic wywalanie getStar i uruchamoac w monadzie - wtedy monady wewnarz nie musza byc MonadFix!
    -- monadfix jest monada uruchamiajaca wszystko i monady

instance HasAST (a t) ast => HasAST (Typed a t) ast where ast = undefined

instance HasAST' a ast => HasAST' (Typed a) ast where ast' = undefined


--instance (LayerGen t m b, MonadStarBuilder t m) => LayerGen t m (Typed b) where
--    genLayers a = Typed <$> StarBuilder.get <*> genLayers a

instance (LayerGen (MuRef ref t) m l, LayeredStarBuilder m ref t) => LayerGen (MuRef ref t) m (Typed l) where
    genLayers a = Typed <$> getStar <*> genLayers a


type MonadStarBuilder' m ref t = (MonadFix m, MonadStarBuilder (Maybe(MuRef ref t)) m)
type LayeredStarBuilder m ref t = (LayeredASTCons Star m ref t, MonadStarBuilder' m ref t)


getStar :: LayeredStarBuilder m ref t => m (MuRef ref t)
getStar = do
    s <- StarBuilder.get
    case s of
        Just    ref -> return ref
        Nothing     -> newStar
    where newStar = mdo oldstar <- StarBuilder.get
                        StarBuilder.put (Just ref)
                        ref <- star'
                        StarBuilder.put oldstar
                        return ref

--newStar :: LayeredStarBuilder m ref t => m (MuRef ref t)
--newStar = mdo StarBuilder.put (Just ref)
--              ref <- star'
--              StarBuilder.put Nothing
--              return ref


--star' :: LayeredASTCons Star m ref a => m (MuRef ref a)
--star' = layeredASTCons Star

-- === Function ===

--
data Foo = Foo deriving (Show)
instance Default Foo where def = Foo
--



type FunctionGraph = Function (HomoNet (Label Foo) Term)

--mkStarType :: _ => _
--mkStarType = mdo
--    ref <- starWith (genAST . Typed ref)
--    return ref

--mkStarType2 :: _ => StarBuilderT
--                   (MuRef ref a)
--                   (GraphBuilder g)
--                   out
--                 -> BldrState g -> (out, g)

--mkStarType2 graph gs = flip runGraph gs $ flip StarBuilder.evalT undefined $ mdo
--    StarBuilder.put ref
--    ref <- star'
--    graph


-- !!! moduly i klasy etc sa specjalnymi rodzajami obiektow, ktore sa w specjalnym slowniku Star typu!

-----------------

--data Strictness = Lazy
--                | Strict
--                deriving (Show)
-- Thunk

        --data Typed a = Typed { _typedData :: a
        --                     } deriving (Show)

        --makeLenses ''Typed

        --instance HasContainer a c => HasContainer (Typed a) c where
        --    container = typedData . container

        --instance HasAST a ast => HasAST (Typed a) ast where
        --    ast = typedData . ast

        --instance {-# OVERLAPPABLE #-} (ASTGen ast m a, Functor m) => ASTGen ast m (Typed a) where
        --    genAST a = Typed <$> genAST a

-- ===================================================================

tst :: (ast ~ Term' (MuRef ref a), a ~ Typed Term', MonadFix m) => (MuRefData ref a -> m (MuRef ref a)) -> ast -> m (MuRef ref a)
tst fref ast = mdo
    x <- fref (Typed x ast)
    return x

tst2 :: MonadFix m => (a -> m a) -> m a
tst2 f = mdo
    x <- f x
    return x

--mkMuRef :: MuRefBuilder m a => a (MuRef a) -> m (MuRef a)

-- class HasAST el ast => ASTGen ast m el where
--    genAST :: ast -> m el

--mkASTRefWith3 f a = mkMuRef =<< f (cons a)





--type ASTGenerator ref m a = MuRefData ref a -> MuRef ref a -> m (MuRefData ref a)

--tst4 :: _ => _ -- ASTGenerator ref m a -> MuRefData ref a -> m (MuRef ref a)
--tst4 f a = mdo
--    ast <- f a t
--    t   <- mkMuRef ast
--    return t

--tst5 :: (MonadFix m, MuRefBuilder ref m a, ASTGen2 v m a (MuRef ref a)) => v (MuRef ref a) -> m (MuRef ref a)
--tst5 = tst4 genAST2

--f :: FunctionGraph
--f = flip execFunctionBuilder def $ do
--    a <- var "a"
--    x <- var "x" @. "foo"
--    y <- x @$ [arg a]

--    return ()

--f' = flip execFunctionBuilder (rebuild f) $ do
--    b <- var "b"
--    return ()

--nx1 :: _ => m (Ref' Term' (Mu (Ref' Term')))
--nx1 = var2 "foo"

--nx2 :: (Ref' Term' (Mu (Ref' Term')), HomoGraph (Term' (Mu (Ref' Term'))))
--nx2 = runGraph nx1 def

ny1 :: LayeredASTCons Var m ref a => m (MuRef ref a)
ny1 = varx' "foo"

--type MyTerm = Labeled Int Term'
    --type MyTerm = Typed Term'
    --ny2 :: _ => (MuRef (Ptr Int) MyTerm, (HomoGraph (MuRefData (Ptr Int) MyTerm)))

runGraph' :: _ => GraphBuilderT g (StarBuilder (Maybe a)) a1 -> BldrState g -> (a1, g)
runGraph' g gs = flip StarBuilder.eval Nothing $ runGraphT g gs

--ny2 :: _ => (MuRef (Ptr Int) (Typed Term'), HomoGraph (MuRefData (Ptr Int) (Typed Term')))
ny2 :: (MuRef (Ptr Int) (Typed Term'), HomoGraph (MuRefData (Ptr Int) (Typed Term')))
ny2 = runGraph' ny1 def

nytst :: (MuRef (Ptr Int) (Typed Term'), HomoGraph (MuRefData (Ptr Int) (Typed Term')))
nytst = flip runGraph' def $ do
    --v1 <- teq (varx' "foo") (undefined :: m (MuRef (Ptr Int) a))
    --v2 <- teq (varx' "bar") (undefined :: m (MuRef (Ptr Int) a))
    v1 <- varx' "foo"
    v2 <- varx' "bar"
    --let x = teq v1 ()
    return v1
    --return (v1, v2)
    ----x2 :: _ => m (GraphRef l Term)
    --x2 = app (int 5) [arg $ int 6]

    ----type MuPtr ptr a

    --mkRef2' :: RefBuilder2 a (Mu (ptr a)) m ptr => a (Mu (ptr a)) -> m (Mu (ptr a)) --a (Mu (ptr a)) => m (Mu (ptr a))
    --mkRef2' = fmap Mu . mkRef2


teq :: a -> a -> a
teq = const

----x3 :: (Constructor Lit ast, GraphRefBuilder el m Foo Term, ASTGen ast m el, m ~ (GraphBuilderT (HomoNet Foo Term) Identity))
----   => GraphBuilderT (HomoNet Foo Term) Identity) (GraphRef Foo Term)
----x3 :: (Constructor Lit ast, ASTGen ast m el, RefBuilder el m (Ref (GraphPtr (Label Foo)) Term)) => m (GraphRef (Label Foo) Term)
----x3 = int 3

------x4 :: GraphBuilderT (HomoNet (Label Foo) Term) Identity (GraphRef (Label Foo) Term)
----x4 = int 3


--newtype Nest m n a = Nest { _nest :: m (n a) } deriving (Show)

--makeLenses ''Nest

--type family Nested (wrappers :: [* -> *]) where
--    Nested '[m]      = m
--    Nested (m ': ms) = Nest m (Nested ms)

----Nest m n a
----Nest m (Nest n l) a

--type GraphRef' nest   = GraphRef (Nested nest)
--type HomoNet'  nest a = HomoNet  (Nested nest) a

--instance Repr (m (n a)) => Repr (Nest m n a) where
--    repr = repr . view nest

----TODO[wd]: template haskellize
----instance Wrapper (Nest m n a) where
----    content = lens (\(Nest a) -> a) Nest


--instance {-# OVERLAPPABLE #-} (Functor m, ASTGen ast m (t (k a))) => ASTGen ast m (Nest t k a) where
--    genAST a = Nest <$> genAST a

--instance HasAST (t (k a)) ast => HasAST (Nest t k a) ast where
--    ast = nest . ast

----y :: (GraphRef' '[Label Int, Typed] Term, HomoNet' '[Label Int, Typed] Term)
--y :: (GraphRef' '[Label Int] Term, HomoNet' '[Label Int] Term)
--y = runGraph x2 def





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
    --putStrLn $ repr y
    putStrLn $ repr $ ny2
    --print c'
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





