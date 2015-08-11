
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

module Tmp.TypecheckerTest2 where

import Flowbox.Prelude hiding (simple, empty, Indexable, Simple, cons, lookup, index, Wrapped, children, Cons)
import Data.Repr

--import qualified Luna.Inference.Type as Type
--import           Luna.Inference.Type
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Text.Lazy      as Text
import           Data.Text.Lazy      (Text)

--import           Luna.Inference.RawData



-- import           GHC.Prim (Any)
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


-- === Term ===


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




--type Ref a h = h (a h)


-- Component types

newtype Var        = Var      Name      deriving (Show, Eq)
data    Cons     a = Cons     Name [a]  deriving (Show)
data    Accessor a = Accessor Name a    deriving (Show)
data    App      a = App      a [Arg a] deriving (Show)

data    Arg      a = Arg { __aname :: Maybe Name , __arec :: a } deriving (Show)


      --termCons :: ConsOf Term a => a -> Term
      --termCons = cons

instance Repr a => Repr (App a) where
    repr (App a args) = "App (" <> repr a <> ") " <> repr args

instance Repr a => Repr (Arg a) where
    repr (Arg n a) = "Arg (" <> repr n <> ") (" <> repr a <> ")"


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

--newtype Val       a = Val       { __valRec       :: Record (ValElems  a)      } deriving (Show)
--newtype ConstTerm a = ConstTerm { __constTermRec :: Record (ConstTermElems a) } deriving (Show)
--newtype Term      a = Term      { __termRec      :: Record (TermElems a)      } deriving (Show)


--newtype ConstTerm a = ConstTerm (Record (ConstTermElems a))
--newtype Term      a = Term      (Record (TermElems a))

--newtype Foo = Foo (Record '[Val, ConstTerm Foo])
--newtype Bar = Bar (Record '[Val, ConstTerm Foo, Term Bar])

type ValTypes       h = ValElems (h (Val h))
type ConstTermTypes h = (Val h) ': ConstTermElems (h (ConstTerm h))
type TermTypes      h = (Val h) ': (ConstTerm h) ': TermElems (h (Term h))

newtype Val       h = Val       { __vrecH :: Record (ValTypes h) }
newtype ConstTerm h = ConstTerm { __crecH :: Record (ConstTermTypes h) }
newtype Term      h = Term      { __trecH :: Record (TermTypes h) }


deriving instance (Show (h (Val h)))                                            => Show (Val h)
deriving instance (Show (h (Val h)), Show (h (ConstTerm h)))                    => Show (ConstTerm h)
deriving instance (Show (h (Val h)), Show (h (ConstTerm h)), Show (h (Term h))) => Show (Term h)


type instance Variants (Val h)       = ValTypes       h
type instance Variants (ConstTerm h) = ConstTermTypes h
type instance Variants (Term h)      = TermTypes      h


makeLenses ''Val
makeLenses ''ConstTerm
makeLenses ''Term

instance IsVariant (Val h)       where record  = _vrecH
                                       variant = Val

instance IsVariant (ConstTerm h) where record  = _crecH
                                       variant = ConstTerm

instance IsVariant (Term h)      where record  = _trecH
                                       variant = Term

--class IsVariant a where
--    variant :: Record (Variants a) -> a
--    record  :: Lens' a (Record (Variants a))


    --type ValTypes       = ValElems Val
    --type ConstTermTypes = Val ': ConstTermElems ConstTerm
    --type TermTypes      = Val ': ConstTerm ': TermElems Term

    --newtype Val       = Val       { __vrec :: Record ValTypes       } deriving (Show)
    --newtype ConstTerm = ConstTerm { __crec :: Record ConstTermTypes } deriving (Show)
    --newtype Term      = Term      { __trec :: Record TermTypes      } deriving (Show)

    --type instance Variants Val       = ValTypes
    --type instance Variants ConstTerm = ConstTermTypes
    --type instance Variants Term      = TermTypes
    --type instance Variants (Arg a)   = Variants a

    --makeLenses ''Val
    --makeLenses ''ConstTerm
    --makeLenses ''Term
    --makeLenses ''Arg

    --instance IsVariant Val       where record  = _vrec
    --                                   variant = Val

    --instance IsVariant ConstTerm where record  = _crec
    --                                   variant = ConstTerm

    --instance IsVariant Term      where record  = _trec
    --                                   variant = Term



    --instance Repr Val       where repr (Val a)       = "Val ("       <> repr a <> ")"
    --instance Repr ConstTerm where repr (ConstTerm a) = "ConstTerm (" <> repr a <> ")"
    --instance Repr      Term where repr (Term a)      = "Term ("      <> repr a <> ")"

--instance IsVariant a => IsVariant (Arg a) where record = _arec . record

--newtype Val         = Val       (Record (ValTypes Val))
--newtype Foo2        = Foo2      (Record (ConstTermTypes Foo2))

data Label l a = Label { _lab :: l, _el :: a } deriving (Show)

makeLenses ''Label

--val :: Constructor a Val => a -> Val
--val = cons

val :: Constructor a (Val h) => a -> (Val h)
val = cons

--val ::

--val :: Constructor a (Val h) => a -> Val h
--val = cons

--x = val (Int 0) :: Label (Val Label)



--class Specialize a where
--    specialize :: Specialization a -> a


class IsConstTerm a b | b -> a where
    constTerm' :: a -> b

--constTerm :: Constructor a ConstTerm => a -> ConstTerm
--constTerm = cons

--int :: forall a b. (Constructor Lit (ValOf a), Constructor (ValOf a) a, Convertible' a b) => Int -> b
--int i = convert' $ cons (cons $ Int i :: ValOf a)


intCons :: forall a h. Constructor (Val h) (a h) => Int -> a h
intCons i = cons (cons $ Int i :: Val h)

--int = mkASTRef . intCons

--x = intCons 5 :: Term Label
--y = cons $ Int 5 :: Term Label


--var :: RecBuilder el m h Term => Name -> m (Ref h Term)
--var = mkASTRef . Var

--newtype Ref     h a = Ref { fromGraphRef :: h (a h) }



--x = int 5 :: Label Val

--instance Convertible' Val Val where convert' = id
instance Convertible' (Val h) (Val h) where convert' = id
instance Convertible' (Term h) (Term h) where convert' = id
--instance Convertible' a (Label a) where convert' = Label
--instance Convertible a (Label a) where convert = Label
--int = cons . val . Int

type family ConstTermOf a
type family ValOf a

--type instance ValOf Val = Val
type instance ValOf (Val h) = Val h
type instance ValOf (Term h) = Val h

--type instance ConstTermOf Term = ConstTerm
type instance ConstTermOf (Term h) = ConstTerm h

--app :: Constructor (App a) a => a -> [Arg a] -> a
--app :: (Constructor (ConstTermOf a) a) => ConstTermOf a -> [Arg (ConstTermOf a)] -> a
--app :: forall a. (Constructor (App (ConstTermOf a)) (ConstTermOf a), Constructor (ConstTermOf a) a) => ConstTermOf a -> [Arg (ConstTermOf a)] -> a
--app a args = cons $ (cons $ (App a args :: App (ConstTermOf a)) :: ConstTermOf a)

--app a args = constTerm $ (App a args)

--x :: Term Label
--x = app (int 1) [arg $ int 2]

--instance IsConstTerm (App Term) Term where
--    constTerm' a = cons (cons a :: ConstTerm)

--class SmartConstructor a b where
--    smartCons :: a -> b

--instance SmartConstructor a a where
--    smartCons = id

----instance

class AsConstructor t a b where
    consAs :: Proxy t -> a -> b

instance {-# OVERLAPPABLE #-} Constructor a t => AsConstructor t a t where
    consAs _ = cons

instance {-# OVERLAPPABLE #-} (Constructor a t, Constructor t b) => AsConstructor t a b where
    consAs _ a = cons (cons a :: t)





--var :: Constructor Var a => Name -> a

--x = var "foo" :: Val

--int :: Constructor Lit a => Int -> a
--int = cons . Int

----int :: Int -> Val
--int i = cons (cons $ Int i :: Val)

--recCons :: (Constructor (Record (Variants a)) a) => Record (Variants a) -> a
--recCons = cons
----app :: Constructor (App a) a => a -> Arg a -> a
--app :: Constructor (App a) a => a -> [Arg a] -> a
--app a args = (cons $ App a args)

----v :: _ => _
--v = app (int 1) [arg $ int 2]

----app2 = recCons .: App

--acc = cons .: Accessor

--type instance Variants Val

--tx :: Constructor Var a => App a
--tx = App (var "foo") [arg $ var "bar"]

--ty :: _ => _
--ty = app (var "foo") [arg $ var "bar"]
--t1 :: Constructor Lit a => Arg a
--t1 = (arg $ int 6)

--v = ty :: Term


--t2 = Term $ app (int 5) [arg $ int 6]

--t1 = Term $ var "foo"

--v1 = Var "name"

--ConstTermNode = Record '[ValTypes]

--type

--newtype Val     = Val   { __valRec   :: Record (ValTypes  Val)           } deriving (Show)
--newtype Term h = Term { __termRec :: Record (TermTypes (Ref Term h)) }
--newtype Val  h = Val  { __valRec  :: Record (ValTypes  (Ref Val  h)) }


--data Node h = ValNode       (Record (ValTypes       (Ref Val h)))
--            | ConstTermNode (Record (ConstTermTypes (Ref Node h)))
--            | TermNode      (Record (TermTypes      (Ref Node h)))



    --newtype Term    = Term  { __termRec  :: Record (TermTypes Term)          } deriving (Show)
    --newtype Val     = Val   { __valRec   :: Record (ValTypes  Val)           } deriving (Show)
    --newtype Term h = Term { __termRec :: Record (TermTypes (Ref Term h)) }
    --newtype Val  h = Val  { __valRec  :: Record (ValTypes  (Ref Val  h)) }


    --data Node h = ValNode       (Record (ValTypes       (Ref Val h)))
    --            | ConstTermNode (Record (ConstTermTypes (Ref Node h)))
    --            | TermNode      (Record (TermTypes      (Ref Node h)))


    --makeLenses ''Term
    --makeLenses ''Val
    --makeLenses ''Term
    --makeLenses ''Val

    ---- instances

    ----deriving instance (Show (Ref Node h), Show (Ref Val h)) => Show (Node h)

    --type instance Variants Val       = ValTypes  Val
    --type instance Variants Term      = TermTypes Term
    --type instance Variants (Term h) = TermTypes (Ref Term h)


    --instance IsVariant Term      where variant = Term
    --                                   record  = _termRec

    --instance IsVariant (Term h) where variant = Term
    --                                   record  = _termRec

    --instance IsVariant Val       where variant = Val
    --                                   record  = _valRec

--instance IsVariant (Term h) where variant = Term
--                                   record  = _termRec

      ----instance Convertible (Val h) (Node h) where convert = ValNode

      --data Typex = Typex deriving (Show) -- fixme

      --data CTVal a = CTVal Typex Ctx a deriving (Show)

--main = V.main

--class HasAST a ast (h :: * -> *) | a -> ast h where
--    ast :: a -> ast h

--instance HasAST (Term h) Term h where
--    ast = id

--instance HasAST a ast h => HasAST (Val a) ast h where
--    ast (Val _ _ a) = ast a

--class AST a where
--    children :: a h -> [h (a h)]

--instance AST Term where
--    children = \case
--        Accessor _ base    -> [base]
--        App      base args -> base : fmap (view term) args
--        _                  -> []


--instance Repr (Term h) where
--    repr = \case
--        Var      n   -> "Var "      <> show n
--        Cons     n   -> "Cons "     <> show n
--        Accessor n _ -> "Accessor " <> show n
--        App      {}  -> "App"

--instance Repr a => Repr (Val a) where
--    repr (Val _ _ a) = repr a



--instance Convertible' (Term h) (Val (Term h)) where convert' = Val Typex UnknownCtx
--instance Convertible' (Term h) (Term h)       where convert' = id



--- === Graph ===

newtype HeteroGraph   = HeteroGraph { __hetReg :: Hetero' Vector } deriving (Show, Default)
newtype HomoGraph   a = HomoGraph   { __homReg :: Vector a       } deriving (Show, Default)

makeLenses ''HeteroGraph
makeLenses ''HomoGraph

instance HasContainer HeteroGraph   (Hetero' Vector) where container = _hetReg
instance HasContainer (HomoGraph a) (Vector a)       where container = _homReg



--- === Graph builders ===

type GraphBuilderT g m = StateT (BldrState g) m
type GraphBuilder  g   = GraphBuilderT g Identity

data BldrState g = BldrState { _orphans :: [Int]
                             , _graph   :: g
                             }

makeClassy ''BldrState


class Monad m => MonadBldrState g m | m -> g where
    getBldrState :: m (BldrState g)
    putBldrState :: (BldrState g) -> m ()

-- utils

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

instance Default g => Default (BldrState g) where
    def = BldrState def def

instance Monad m => MonadBldrState g (GraphBuilderT g m) where
    getBldrState = get
    putBldrState = put



---- === Ref ===

type Rec h a = h (a h)

newtype Ref     h a = Ref { fromRef :: Rec h a }
type    Wrapped m a = m (a (HPtr Int m))
type    Simple    a = a (Ptr Int)

-- utils

class Monad m => ToMRef t m l a | t -> l a where
    toMRef :: t -> m (GraphRef l a)

-- instances

instance                             (Monad m) => ToMRef    (GraphRef l a)  m l a where toMRef = return
instance {-# OVERLAPPABLE #-} (m ~ n, Monad m) => ToMRef (m (GraphRef l a)) n l a where toMRef = id


deriving instance Show (h (a h)) => Show (Ref h a)



-- === RecBuilder ===

class Monad m => RecBuilder m h a where
    mkRec :: a h -> m (Ref h a)


instance (Convertible idx (h (a h)), HasContainer g cont, Appendable cont idx el, Monad m, PtrTarget h a el, MkEl (a h) m el)
      => RecBuilder (GraphBuilderT g m) h a where
    mkRec a = do
        el <- lift $ mkEl a
        fmap (Ref . convert) . withGraph . append $ (el :: el)

type NetRecBuilder m l a = RecBuilder m (GraphPtr l) a
type GraphConstructor base l a = Constructor base (GraphNode l a)


mkRef :: NetRecBuilder m l a => GraphNode l a -> m (GraphRef l a)
mkRef = fmap GraphRef . mkRec

class MkEl a m b where
    mkEl :: a -> m b


instance {-# OVERLAPPABLE #-} (Applicative m, LabBuilder m l) => MkEl a m (Label l a) where
    mkEl a = Label <$> mkLabel <*> pure a

class LabBuilder m l where
    mkLabel :: m l

instance {-# OVERLAPPABLE #-} (Monad m, Default a) => LabBuilder m a where
    mkLabel = return def


var :: (NetRecBuilder m l a, GraphConstructor Var l a) => Name -> m (GraphRef l a)
var = mkRef . cons . Var


int :: (NetRecBuilder m l a, GraphConstructor Lit l a) => Int -> m (GraphRef l a)
int = mkRef . cons . Int


app :: (NetRecBuilder m l a, HConstructor App l a, ToMRef t m l a) => t -> [ArgRef m l a] -> m (GraphRef l a)
app base args = do
    baseRef <- toRawMRef base
    argRefs <- mapM readArgRef args
    mkRef . cons $ App baseRef argRefs
    where readArgRef (Arg n mref) = Arg n . fromRef . fromGraphRef <$> mref


accessor :: (NetRecBuilder m l a, HConstructor Accessor l a, ToMRef t m l a) => Name -> t -> m (GraphRef l a)
accessor n r = mkRef . cons . Accessor n =<< toRawMRef r

type HConstructor base l a = Constructor (base ((GraphPtr l) (GraphNode l a))) (GraphNode l a)


(@.) = flip accessor
(@$) = app

arg = Arg Nothing . toMRef


--x :: (HConstructor App h a, Constructor Lit (a h), RecBuilder m h a) => m (Ref h a)


x2 :: (NetRecBuilder m l Term) => m (GraphRef l Term)
x2 = app (int 5) [arg $ int 6]

newtype GraphRef l a = GraphRef { fromGraphRef :: Ref (GraphPtr l) a } deriving (Show)

type GraphPtr  l   = HPtr Int (Label l)
type GraphNode l a = a (GraphPtr l)
type HomoNet   l a = HomoGraph ((Label l) (GraphNode l a))
type HeteroNet     = HeteroGraph


-- y :: (GraphRef Ctx Term, HomoNet Ctx Term)
-- y = runGraphBuilder x2

-- (a,b) = y

-- c = elems b

data Ctx = Ctx Int deriving (Show)
instance Default Ctx where def = Ctx 1

-- instance Monad m => LabBuilder m Ctx where
--     mkLabel = return $ Ctx 3

instance (MonadState Ctx m) => LabBuilder m Ctx where
    mkLabel = get


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


f :: FunctionGraph
f = runIdentity $ flip evalStateT (Ctx 0) $ runFunctionBuilderT $ do
    lift $ put $ Ctx 2
    a <- var "a"
    lift $ put $ Ctx 3
    -- b <- withCtx (Ctx 7) $ var "b"
    x <- var "x" @. "foo"
    lift $ put $ Ctx 4
    y <- x @$ [arg a]

    return ()

    -- a <- withCtx 11 $ app (withCtx 12 $ var "a") (withCtx 13 $ var "b")

withCtx id f = do
    ctx <- get
    put id
    out <- f
    put ctx
    return out


main = do
    -- putStrLn $ repr y
    putStrLn $ repr f
    return ()




    -- put 2
    -- a <- withID 2 $ var "a"

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
--    a <- ref_ $ var2 "a"
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





