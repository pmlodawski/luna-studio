
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
import Control.Monad.State hiding (withState, mapM)


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
import Flowbox.System.Types

type ID = Int


--- === Ctx ===

--data Ctx a = Pure
--           | IO       (Maybe a)
--           | Unknown  (Maybe a)
--           | Ctx Name (Maybe a)
--           deriving (Show)

data Base = Pure
          | IO
          deriving (Show)

--data Ctx = Ctx Name deriving (Show)

--data Ctx = KnownCtx Base [Name]
--         | UnknownCtx
--         deriving (Show)

-- === Literals ===

data Lit = Int Int
         | String Text.AutoBuilder
         deriving (Show)

instance IsString Lit where
    fromString = String . fromString


-- === Terms ===


type Name = String


--type Term h = h (Term h)


--data Arg h = Arg { _label :: Maybe Name, _term :: Term h }

-- wszystkie rzeczy nazwane powinny byc w slownikach w jezyku! - czyli datatypy ponizej zostaja,
-- ale mozemy tworzyc np. Var funkcjami, ktore oczekuja konkretnego Stringa!
--data Term h = Var      Name
--            | Cons     Name
--            | Accessor Name (Term h)
--            | App      (Term h) [Arg h]
--            | Lambda
--            | RecUpd
--            | Unify    (Term h) (Term h)
--            | Case
--            | Typed
--            -- | Assignment
--            -- x | Decons
--            | Curry
--            -- | Meta
--            -- | Tuple
--            -- | Grouped
--            -- | Decl
--            | Lit      Lit
--            | Wildcard
--            -- | Tuple
--            -- | List
--            | Unsafe [Name] (Term h)




-- Component types

newtype Var        = Var      Name      deriving (Show, Eq)
data    Cons     a = Cons     Name [a]  deriving (Show)
data    Accessor a = Accessor Name a    deriving (Show)
data    App      a = App      a [Arg a] deriving (Show)

data    Arg      a = Arg { __aname :: Maybe Name , __arec :: a } deriving (Show)


-- Type sets

type TermElems      a = Var
                     ': ConstTermElems a

type ConstTermElems a = Accessor a
                     ': App      a
                     ': ValElems a

type ValElems       a = Lit
                     ': Cons a
                     ': '[]


-- Record types

type ValTypes       h = ValElems (h (Val h))
type ConstTermTypes h = (Val h) ': ConstTermElems (h (ConstTerm h))
type TermTypes      h = (Val h) ': (ConstTerm h) ': TermElems (h (Term h))

newtype Val       h = Val       { __vrecH :: Record (ValTypes h) }
newtype ConstTerm h = ConstTerm { __crecH :: Record (ConstTermTypes h) }
newtype Term      h = Term      { __trecH :: Record (TermTypes h) }

makeLenses ''Val
makeLenses ''ConstTerm
makeLenses ''Term

-- instances

deriving instance (Show (h (Val h)))                                            => Show (Val h)
deriving instance (Show (h (Val h)), Show (h (ConstTerm h)))                    => Show (ConstTerm h)
deriving instance (Show (h (Val h)), Show (h (ConstTerm h)), Show (h (Term h))) => Show (Term h)

type instance Variants (Val h)       = ValTypes       h
type instance Variants (ConstTerm h) = ConstTermTypes h
type instance Variants (Term h)      = TermTypes      h

instance IsVariant (Val h)       where record  = _vrecH
                                       variant = Val

instance IsVariant (ConstTerm h) where record  = _crecH
                                       variant = ConstTerm

instance IsVariant (Term h)      where record  = _trecH
                                       variant = Term

-- Repr instances

instance Repr a => Repr (App a) where
    repr (App a args) = "App (" <> repr a <> ") " <> repr args

instance Repr a => Repr (Arg a) where
    repr (Arg n a) = "Arg (" <> repr n <> ") (" <> repr a <> ")"



-- === HasAST ===

class HasAST a ast | a -> ast where
  ast :: Lens' a ast

instance HasAST (Val h)       (Val h)       where ast = id
instance HasAST (ConstTerm h) (ConstTerm h) where ast = id
instance HasAST (Term h)      (Term h)      where ast = id


class HasAST el ast => ASTGen ast m el where
    genAST :: ast -> m el


instance Monad m => ASTGen (Val h)       m (Val h)       where genAST = return
instance Monad m => ASTGen (ConstTerm h) m (ConstTerm h) where genAST = return
instance Monad m => ASTGen (Term h)      m (Term h)      where genAST = return



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



--- === Graph ===

newtype HeteroGraph   = HeteroGraph { __hetReg :: Hetero' Vector } deriving (Show, Default)
newtype HomoGraph   a = HomoGraph   { __homReg :: Vector a       } deriving (Show, Default)

makeLenses ''HeteroGraph
makeLenses ''HomoGraph

instance HasContainer HeteroGraph   (Hetero' Vector) where container = _hetReg
instance HasContainer (HomoGraph a) (Vector a)       where container = _homReg


newtype GraphRef l a = GraphRef { fromGraphRef :: Ref (GraphPtr l) a } deriving (Show)

type GraphPtr  l   = HPtr Int (Label l)
type GraphNode l a = a (GraphPtr l)
type HomoNet   l a = HomoGraph ((Label l) (GraphNode l a))
type HeteroNet     = HeteroGraph



---- === Ref ===

type Rec h a = h (a h)

newtype Ref     h a = Ref { fromRef :: Rec h a }
type    Wrapped m a = m (a (HPtr Int m))
type    Simple    a = a (Ptr Int)

-- utils

class Monad m => ToMRef t m l a | t -> l a where
    toMRef :: t -> m (GraphRef l a)

-- instances

deriving instance Show (h (a h)) => Show (Ref h a)

instance                             (Monad m) => ToMRef    (GraphRef l a)  m l a where toMRef = return
instance {-# OVERLAPPABLE #-} (m ~ n, Monad m) => ToMRef (m (GraphRef l a)) n l a where toMRef = id



-- === RefBuilder ===

class Monad m => RefBuilder el m ref | el m -> ref, ref m -> el where
    mkRef :: el -> m ref



--- === Graph builders ===

type GraphBuilderT g m = StateT (BldrState g) m
type GraphBuilder  g   = GraphBuilderT g Identity

data BldrState g = BldrState { _orphans :: [Int]
                             , _graph   :: g
                             }

makeClassy ''BldrState

type GraphRefBuilder el m l a = RefBuilder el m (Ref (GraphPtr l) a)
type GraphConstructor base l a ast = Constructor (base (Rec (GraphPtr l) a)) ast

class Monad m => MonadBldrState g m | m -> g where
    getBldrState :: m (BldrState g)
    putBldrState :: (BldrState g) -> m ()

-- graph utils

mkGraphRef :: GraphRefBuilder el m l a => el -> m (GraphRef l a)
mkGraphRef = fmap GraphRef . mkRef

withBldrState_ :: MonadBldrState g m => (BldrState g -> BldrState g) -> m ()
withBldrState_ f = withBldrState $ fmap (,()) f

withBldrState :: MonadBldrState g m => (BldrState g -> (BldrState g, a)) -> m a
withBldrState f = do
    bldr <- getBldrState
    let (bldr', out) = f bldr
    putBldrState $ bldr'
    return out

withGraph :: MonadBldrState g m => (g -> (g, a)) -> m a
withGraph = withBldrState . mapOver graph

runGraphBuilderT :: (Monad m, Default g) => GraphBuilderT g m a -> m (a, g)
runGraphBuilderT gb = over _2 (view graph) <$> runStateT gb def

execGraphBuilderT :: (Monad m, Default g) => GraphBuilderT g m a -> m g
execGraphBuilderT gb = snd <$> runGraphBuilderT gb

evalGraphBuilderT :: (Monad m, Default g) => GraphBuilderT g m a -> m a
evalGraphBuilderT gb = fst <$> runGraphBuilderT gb

runGraphBuilder :: Default g => GraphBuilder g a -> (a, g)
runGraphBuilder = runIdentity . runGraphBuilderT

execGraphBuilder :: Default g => GraphBuilder g a -> g
execGraphBuilder = runIdentity . execGraphBuilderT

evalGraphBuilder :: Default g => GraphBuilder g a -> a
evalGraphBuilder = runIdentity . evalGraphBuilderT

-- instances

instance (Convertible idx (h (a h)), HasContainer g cont, Appendable cont idx el, Monad m, PtrTarget h a el)
      => RefBuilder el (GraphBuilderT g m) (Ref h a) where
    mkRef el = fmap (Ref . convert) . withGraph . append $ el

instance Default g => Default (BldrState g) where
    def = BldrState def def

instance Monad m => MonadBldrState g (GraphBuilderT g m) where
    getBldrState = get
    putBldrState = put




--- === AST builders ===

-- generic builders

mkASTRefWith :: (Constructor variant ast, GraphRefBuilder el m l a) => (ast -> m el) -> variant -> m (GraphRef l a)
mkASTRefWith f a = mkGraphRef =<< f (cons a)


varWith :: (Constructor Var ast, GraphRefBuilder el m l a) => (ast -> m el) -> Name -> m (GraphRef l a)
varWith f = mkASTRefWith f . Var

intWith :: (Constructor Lit ast, GraphRefBuilder el m l a) => (ast -> m el) -> Int -> m (GraphRef l a)
intWith f = mkASTRefWith f . Int

accessorWith :: (GraphConstructor Accessor l a ast, GraphRefBuilder el m l a, ToMRef t m l a)
             => (ast -> m el) -> Name -> t -> m (GraphRef l a)
accessorWith f n r = mkASTRefWith f . Accessor n =<< toRawMRef r

appWith :: (GraphConstructor App l a ast, GraphRefBuilder el m l a, ToMRef t m l a)
        => (ast -> m el) -> t -> [ArgRef m l a] -> m (GraphRef l a)
appWith f base args = do
    baseRef <- toRawMRef base
    argRefs <- mapM readArgRef args
    mkASTRefWith f $ App baseRef argRefs
    where readArgRef (Arg n mref) = Arg n . fromRef . fromGraphRef <$> mref

-- specific builders

var :: (Constructor Var ast, GraphRefBuilder el m l a, ASTGen ast m el) => Name -> m (GraphRef l a)
var = varWith genAST

int :: (Constructor Lit ast, GraphRefBuilder el m l a, ASTGen ast m el) => Int -> m (GraphRef l a)
int = intWith genAST

app :: (GraphConstructor App l a ast, GraphRefBuilder el m l a, ToMRef t m l a, ASTGen ast m el)
    => t -> [ArgRef m l a] -> m (GraphRef l a)
app = appWith genAST

accessor :: (GraphConstructor Accessor l a ast, GraphRefBuilder el m l a, ToMRef t m l a, ASTGen ast m el)
         => Name -> t -> m (GraphRef l a)
accessor = accessorWith genAST

arg :: ToMRef t m l a => t -> Arg (m (GraphRef l a))
arg = Arg Nothing . toMRef

-- operators

(@.) = flip accessor
(@$) = app





-- ===================================================================


--x2 :: _ => m (GraphRef l Term)
x2 = app (int 5) [arg $ int 6]

--x3 :: (Constructor Lit ast, GraphRefBuilder el m Ctx Term, ASTGen ast m el, m ~ (GraphBuilderT (HomoNet Ctx Term) Identity))
--   => GraphBuilderT (HomoNet Ctx Term) Identity) (GraphRef Ctx Term)
x3 :: (Constructor Lit ast, ASTGen ast m el, RefBuilder el m (Ref (GraphPtr Ctx) Term)) => m (GraphRef Ctx Term)
x3 = int 3

--x4 :: GraphBuilderT (HomoNet Ctx Term) Identity (GraphRef Ctx Term)
x4 = int 3




y :: (GraphRef Ctx Term, HomoNet Ctx Term)
y = runGraphBuilder x2

y3 :: (GraphRef Ctx Term, HomoNet Ctx Term)
y3 = runGraphBuilder x4

(a,b) = y

c = elems b

data Ctx = Ctx deriving (Show)
instance Default Ctx where def = Ctx



main = do
    putStrLn $ repr y
    return ()

toRawMRef = fmap (fromRef . fromGraphRef) . toMRef

type ArgRef m l a = Arg (m (GraphRef l a))


-- === Function ===

data Function body = Function { _body :: body } deriving (Show)

type FunctionGraph = Function (HomoNet Ctx Term)

makeLenses ''Function

-- utils

runFunctionBuilderT :: (Monad m, Default g) => GraphBuilderT g m a -> m (Function g)
runFunctionBuilderT gb = do
    (a, g) <- runGraphBuilderT gb
    return $ Function g

runFunctionBuilder :: Default g => GraphBuilder g a -> Function g
runFunctionBuilder = runIdentity . runFunctionBuilderT

-- instances

instance HasContainer body c => HasContainer (Function body) c where
    container = body . container


--f :: FunctionGraph
--f = runFunctionBuilder $ do
--    a <- var "a"
--    x <- var "x" @. "foo"
--    y <- x @$ [arg a]

--    return ()


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

--toGraphViz :: HomoGraph (HomoNet Ctx Term) -> DotGraph Int
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





