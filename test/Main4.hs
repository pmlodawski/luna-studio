
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

module Main where

import Flowbox.Prelude hiding (simple, empty, Indexable, Simple, cons, lookup, index, Wrapped)
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
import Data.Constraint
import Control.Error.Util (hush)
import Data.Convert.Errors (TypeMismatch (TypeMismatch))
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









---------- ?


unsafeAt :: (UncheckedGetIdx m, UncheckedSetIdx m) => Index m -> Lens' m (ElementOf m)
unsafeAt i = lens (uncheckedGetIdx i) (flip $ uncheckedSetIdx i)

mapOver :: (Lens' b a) -> (a -> (a, x)) -> (b -> (b, x))
mapOver lens f s = (s & lens .~ a, out) where
    (a, out) = f $ s ^. lens




--- === Voids ===

data Void = Void deriving (Show)
class    Void1 t1
instance Void1 t1
class    Void2 t1 t2
instance Void2 t1 t2
class    Void3 t1 t2 t3
instance Void3 t1 t2 t3
class    Void4 t1 t2 t3 t4
instance Void4 t1 t2 t3 t4
class    Void5 t1 t2 t3 t4 t5
instance Void5 t1 t2 t3 t4 t5
class    Void6 t1 t2 t3 t4 t5 t6
instance Void6 t1 t2 t3 t4 t5 t6
class    Void7 t1 t2 t3 t4 t5 t6 t7
instance Void7 t1 t2 t3 t4 t5 t6 t7
class    Void8 t1 t2 t3 t4 t5 t6 t7 t8
instance Void8 t1 t2 t3 t4 t5 t6 t7 t8
class    Void9 t1 t2 t3 t4 t5 t6 t7 t8 t9
instance Void9 t1 t2 t3 t4 t5 t6 t7 t8 t9

-- where



--- === [TOEXTRACT] Instances ===
instance Default (Vector a) where def = mempty

castableLens :: IsoCastable a b => Iso' a b
castableLens = iso cast cast

convertibleLens :: IsoConvertible a b => Iso' a b
convertibleLens = iso convert convert





--- === HasContainer ===

class HasContainer a cont | a -> cont where
    container :: Lens' a cont

instance HasContainer [a]           [a]           where container = id
instance HasContainer (Vector a)    (Vector a)    where container = id


--- === Containers ===

type Container idx el cont = (IndexOf el cont ~ idx, ElementByIdx idx cont ~ el)
type family IndexOf      el cont
type family ElementByIdx idx cont
type family IdxType      idx

class Container idx el cont => Appendable2         el idx cont where append2         :: el -> cont -> (cont, idx)
class Container idx el cont => Prependable2        el idx cont where prepend2        :: el -> cont -> (cont, idx)
class Container idx el cont => Updatable           el idx cont where update          :: idx -> el -> cont -> Maybe cont
class Container idx el cont => Insertable          el idx cont where insert          :: idx -> el -> cont -> cont
class Container idx el cont => UnsafeInsertable    el idx cont where unsafeInsert    :: idx -> el -> cont -> cont
class Container idx el cont => UncheckedInsertable el idx cont where uncheckedInsert :: idx -> el -> cont -> cont
class Container idx el cont => Indexable2          el idx cont where index           :: idx -> cont -> Maybe el
class Container idx el cont => UnsafeIndexable     el idx cont where unsafeIndex     :: idx -> cont -> el
class Container idx el cont => UncheckedIndexable  el idx cont where uncheckedIndex  :: idx -> cont -> el

instance {-# OVERLAPPABLE #-} Indexable2       el idx cont => UnsafeIndexable     el idx cont where unsafeIndex     = fromJust .: index
instance {-# OVERLAPPABLE #-} UnsafeIndexable  el idx cont => UncheckedIndexable  el idx cont where uncheckedIndex  = unsafeIndex
instance {-# OVERLAPPABLE #-} Updatable        el idx cont => UnsafeInsertable    el idx cont where unsafeInsert    = fromJust .:. update
instance {-# OVERLAPPABLE #-} UnsafeInsertable el idx cont => UncheckedInsertable el idx cont where uncheckedInsert = unsafeInsert

-- utils

append_ :: Appendable2 el idx cont => el -> cont -> cont
append_ = fst .: append2

prepend_ :: Prependable2 el idx cont => el -> cont -> cont
prepend_ = fst .: prepend2

-- generalized funcs -- TODO: extract

append2' :: (Appendable2 el idx cont, HasContainer a cont) => el -> a -> (a, idx)
append2' el = mapOver container $ append2 el

prepend2' :: (Prependable2 el idx cont, HasContainer a cont) => el -> a -> (a, idx)
prepend2' el = mapOver container $ prepend2 el

append_' :: (Appendable2 el idx cont, HasContainer a cont) => el -> a -> a
append_' el = container %~ append_ el

prepend_' :: (Prependable2 el idx cont, HasContainer a cont) => el -> a -> a
prepend_' el = container %~ prepend_ el

update' :: (Updatable el idx cont, HasContainer a cont) => idx -> el -> a -> Maybe a
update' idx el = container $ update idx el

insert' :: (Insertable el idx cont, HasContainer a cont) => idx -> el -> a -> a
insert' idx el = container %~ insert idx el

unsafeInsert' :: (UnsafeInsertable el idx cont, HasContainer a cont) => idx -> el -> a -> a
unsafeInsert' idx el = container %~ unsafeInsert idx el

uncheckedInsert' :: (UncheckedInsertable el idx cont, HasContainer a cont) => idx -> el -> a -> a
uncheckedInsert' idx el = container %~ uncheckedInsert idx el

index' :: (Indexable2 el idx cont, HasContainer a cont) => idx -> a -> Maybe el
index' idx cont = index idx $ cont ^. container

unsafeIndex' :: (UnsafeIndexable el idx cont, HasContainer a cont) => idx -> a -> el
unsafeIndex' idx cont = unsafeIndex idx $ cont ^. container

uncheckedIndex' :: (UncheckedIndexable el idx cont, HasContainer a cont) => idx -> a -> el
uncheckedIndex' idx cont = uncheckedIndex idx $ cont ^. container



type instance IndexOf el [a]              = Int
type instance IndexOf el (Vector a)       = Int

type instance ElementByIdx idx [a]        = a
type instance ElementByIdx idx (Vector a) = a


--- === Unified values ===

-- FIXME: replace Show and maybe Typeable with Constraint expressions
data Unified ctx where
    Unified :: (ctx a, UnifiedEl a) => a -> Unified ctx

type UnifiedEl a = (Typeable a, Show a)

-- instances

instance Show (Unified ctx) where
    show (Unified a) = show a

instance Typeable a => MaybeConvertible (Unified ctx) TypeMismatch a where
    tryConvert (Unified u) = if tu == ta then Right $ unsafeCoerce u
                                         else Left  $ TypeMismatch tu ta
        where tu = typeOf u
              ta = typeOf (undefined :: a)

instance {-# OVERLAPPABLE #-}                         Castable (Unified ctx) a where cast (Unified a) = unsafeCoerce a
instance {-# OVERLAPPABLE #-} (ctx a, UnifiedEl a) => Castable a (Unified ctx) where cast             = Unified



-- === Ptr ===

newtype Ptr  i   a = Ptr i
newtype HPtr i m a = HPtr (Ptr i (m a)) deriving (Show)

-- injective TF
class    PtrTarget (a :: * -> *) (b :: (* -> *) -> *) c | a b -> c, c -> a b --where
instance PtrTarget (HPtr i h) a {- = -} (h (a (HPtr i h)))
instance PtrTarget (Ptr  i)   a {- = -} (a (Ptr i))

-- instances

instance (Typeable a, Show i) => Show (Ptr i a) where
    show (Ptr i) = "Ptr " <> show i <> " (" <> show (typeOf (undefined :: a)) <> ")"

type instance IdxType (Ptr  i   a) = a
type instance IdxType (HPtr i m a) = a

instance Convertible i (Ptr i a)    where convert = Ptr
instance Convertible (Ptr i a) i    where convert = ptrIdx
instance Convertible i (HPtr i m a) where convert = HPtr . convert
instance Convertible (HPtr i m a) i where convert = ptrIdx

class    PtrIdx p i | p -> i    where ptrIdx :: p -> i
instance PtrIdx (Ptr  i   a) i  where ptrIdx (Ptr i)  = i
instance PtrIdx (HPtr i m a) i  where ptrIdx (HPtr p) = ptrIdx p



--- === Hetero Containers ===

-- TODO: make it PolyKinded
--type family Hetero (ctx :: * -> Constraint) (cont :: * -> *) :: *

type Hetero ctx cont = HeteroContainer (cont (Unified ctx))
type Hetero'    cont = Hetero Void1 cont

newtype HeteroContainer cont = HeteroContainer { _cont :: cont } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''HeteroContainer

-- instances


instance HasContainer (HeteroContainer c) (HeteroContainer c) where container = id


instance Default cont => Default (HeteroContainer cont) where
    def = HeteroContainer def

instance Monoid cont => Monoid (HeteroContainer cont) where
    mempty = HeteroContainer mempty
    (HeteroContainer c) `mappend` (HeteroContainer c') = HeteroContainer $ c <> c'

type HeteroTransCtx idx ctx a cont idx' el = ( Container idx a (HeteroContainer cont)
                                             , ElementOf cont ~ el
                                             , el ~ Unified ctx
                                             , ctx a, UnifiedEl a
                                             , IsoConvertible idx idx'
                                             )

type instance IndexOf el (HeteroContainer cont) = Ptr (Index cont) el
type instance ElementByIdx idx (HeteroContainer cont) = IdxType idx

instance (HeteroTransCtx idx ctx a cont idx' el, Appendable2 el idx' cont)
      => Appendable2 a idx (HeteroContainer cont) where
    append2 a (HeteroContainer cont) = (HeteroContainer cont', convert idx') where
        (cont', idx') = append2 (Unified a :: Unified ctx) cont

instance (HeteroTransCtx idx ctx a cont idx' el, Prependable2 el idx' cont)
      => Prependable2 a idx (HeteroContainer cont) where
    prepend2 a (HeteroContainer cont) = (HeteroContainer cont', convert idx') where
        (cont', idx') = prepend2 (Unified a :: Unified ctx) cont

instance (HeteroTransCtx idx ctx a cont idx' el, Updatable el idx' cont)
      => Updatable a idx (HeteroContainer cont) where
    update idx a = mapM $ update (convert idx) (Unified a :: Unified ctx)

instance (HeteroTransCtx idx ctx a cont idx' el, Insertable el idx' cont)
      => Insertable a idx (HeteroContainer cont) where
    insert idx a = fmap $ insert (convert idx) (Unified a :: Unified ctx)

instance (HeteroTransCtx idx ctx a cont idx' el, UnsafeInsertable el idx' cont)
      => UnsafeInsertable a idx (HeteroContainer cont) where
    unsafeInsert idx a = fmap $ unsafeInsert (convert idx) (Unified a :: Unified ctx)

instance (HeteroTransCtx idx ctx a cont idx' el, UncheckedInsertable el idx' cont)
      => UncheckedInsertable a idx (HeteroContainer cont) where
    uncheckedInsert idx a = fmap $ uncheckedInsert (convert idx) (Unified a :: Unified ctx)

instance (HeteroTransCtx idx ctx a cont idx' el, Indexable2 el idx' cont, MaybeConvertible (Unified ctx) e a)
      => Indexable2 a idx (HeteroContainer cont) where
    index idx (HeteroContainer cont) = hush . tryConvert =<< (index (convert idx) cont :: Maybe (Unified ctx))

instance (HeteroTransCtx idx ctx a cont idx' el, UnsafeIndexable el idx' cont, Castable (Unified ctx) a)
      => UnsafeIndexable a idx (HeteroContainer cont) where
    unsafeIndex idx (HeteroContainer cont) = cast $ (unsafeIndex (convert idx) cont :: (Unified ctx))

instance (HeteroTransCtx idx ctx a cont idx' el, UncheckedIndexable el idx' cont, Castable (Unified ctx) a)
      => UncheckedIndexable a idx (HeteroContainer cont) where
    uncheckedIndex idx (HeteroContainer cont) = cast $ (uncheckedIndex (convert idx) cont :: (Unified ctx))


-- other instances vvv (TODO: refactor)

-- vector instances

instance (a ~ b) => Appendable2         a Int (Vector b) where append2             a v = (v', length v' - 1) where v' = Vector.snoc v a
instance (a ~ b) => Prependable2        a Int (Vector b) where prepend2            a v = (v', 0)             where v' = Vector.cons a v
instance (a ~ b) => Indexable2          a Int (Vector b) where index           idx   v = (Vector.!?)          v idx
instance (a ~ b) => UnsafeIndexable     a Int (Vector b) where unsafeIndex     idx   v = (Vector.!)           v idx
instance (a ~ b) => UncheckedIndexable  a Int (Vector b) where uncheckedIndex  idx   v = (Vector.unsafeIndex) v idx
instance (a ~ b) => UnsafeInsertable    a Int (Vector b) where unsafeInsert    idx a v = (Vector.//)        v [(idx,a)]
instance (a ~ b) => UncheckedInsertable a Int (Vector b) where uncheckedInsert idx a v = (Vector.unsafeUpd) v [(idx,a)]
instance (a ~ b) => Updatable           a Int (Vector b) where update          idx a v = if idx >= 0 && idx < Vector.length v
                                                                                             then Just $ uncheckedInsert idx a v
                                                                                             else Nothing

-- list instances

instance (a ~ b) => Appendable2  a Int [b] where append2      a l = case l of []     -> ([a], 0)
                                                                              (x:xs) -> (x:l', idx' + 1) where
                                                                                  (l', idx') = append2 a xs
instance (a ~ b) => Prependable2 a Int [b] where prepend2     a l = (a:l, 0)
instance (a ~ b) => Indexable2   a Int [b] where index    idx   l = case (idx, l) of
                                                                        (0, (x:_))  -> Just x
                                                                        (_, (_:xs)) -> index (idx - 1) xs
                                                                        _           -> Nothing
instance (a ~ b) => Updatable    a Int [b] where update   idx a l = case (idx, l) of
                                                                        (0, (x:xs)) -> Just $ a : xs
                                                                        (_, (x:xs)) -> (x:) <$> update (idx - 1) a xs
                                                                        _           -> Nothing







--- === Ctx ===

data Ctx a = Pure
           | IO       (Maybe a)
           | Unknown  (Maybe a)
           | Ctx Name (Maybe a)
           deriving (Show)

-- === Literals ===

data Literal = Int Int
             | String Text.AutoBuilder


instance IsString Literal where
    fromString = String . fromString


-- === Expr ===


type Name = String


type HExpr h = h (Expr h)


data Arg h = Arg { _label :: Maybe Name, _arg :: HExpr h }

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
            | Unsafe [Name] (HExpr h)

type Pattern h = HExpr h

data Typex = Typex deriving (Show) -- fixme

data Val a = Val Typex (Ctx a) [Ctx a] a deriving (Show)


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


class IsNode n inp | n -> inp where
    inputs :: n -> [inp]

instance IsNode (Expr h) (HExpr h) where
    inputs = \case
        Accessor _    base -> [base]
        App      base args -> base : fmap (view arg) args
        _                  -> []


instance Convertible' (Expr h) (Val (Expr h)) where convert' = Val Typex Pure []
instance Convertible' (Expr h) (Expr h)       where convert' = id






--- === Builder ===

newtype HeteroGraph   = HeteroGraph { __hetReg :: Hetero' Vector } deriving (Show, Default)
newtype HomoGraph   a = HomoGraph   { __homReg :: Vector a       } deriving (Show, Default)

makeLenses ''HeteroGraph
makeLenses ''HomoGraph

instance HasContainer HeteroGraph   (Hetero' Vector) where container = _hetReg
instance HasContainer (HomoGraph a) (Vector a)       where container = _homReg


type GraphBuilderT g m = StateT (BldrState g) m
type GraphBuilder  g   = GraphBuilderT g Identity

data BldrState g = BldrState { _orphans :: [Int]
                             , _graph   :: g
                             }

makeClassy ''BldrState


class Monad m => MonadBldrState g m | m -> g where
    getBldrState :: m (BldrState g)
    putBldrState :: (BldrState g) -> m ()

instance Default g => Default (BldrState g) where
    def = BldrState def def

instance Monad m => MonadBldrState g (GraphBuilderT g m) where
    getBldrState = get
    putBldrState = put

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


---- === Ref ===

newtype Ref     h a = Ref (h (a h))
type    Wrapped m a = m (a (HPtr Int m))
type    Simple    a = a (Ptr Int)

-- utils

class ToMRef t m h a | t -> h a where
    toMRef :: t -> m (Ref h a)

-- instances

instance                             (Monad m) => ToMRef    (Ref h a)  m h a where toMRef = return
instance {-# OVERLAPPABLE #-} (m ~ n, Monad m) => ToMRef (m (Ref h a)) n h a where toMRef = id



-- === RefBuilder ===

class (Monad m, PtrTarget h a el) => RefBuilder el m h a where
    mkRef :: el -> m (Ref h a)


instance (Convertible idx (h (a h)), HasContainer g cont, Appendable2 el idx cont, Monad m, PtrTarget h a el)
      => RefBuilder el (GraphBuilderT g m) h a where
    mkRef a = fmap (Ref . convert) . withGraph . mapOver container $ append2 a


-- utils

mkASTRef :: (Convertible' (a h) el, RefBuilder el m h a) => a h -> m (Ref h a)
mkASTRef = mkRef . convert'




data Function body = Function { _body :: body } deriving (Show)

type FunctionGraph = Function (HomoGraph (Wrapped Val Expr))

runFunctionBuilderT :: (Monad m, Default g) => GraphBuilderT g m a -> m (Function g)
runFunctionBuilderT gb = do
    (a, g) <- runGraphBuilderT gb
    return $ Function g

runFunctionBuilder :: Default g => GraphBuilder g a -> Function g
runFunctionBuilder = runIdentity . runFunctionBuilderT


var = mkASTRef . Var

f :: FunctionGraph
f = runFunctionBuilder $ do
    a <- var "a"
    return ()

g1 :: HomoGraph (Wrapped Val Expr)
--g1 :: HomoGraph (Simple Expr)
g1 = execGraphBuilder $ do
    a <- var "a"
    --return a
    return ()


--g1 :: HomoGraph _
--g1 = runIdentity $ runGraphBuilder $ do
--    a <- var4
----    --a    <- ref "foo" $ var "a"
----    --b    <- ref "bar" $ accessor "foo" (var "b")
----    --c    <- ref "baz" $ match (var "a") (var "b")
----    return ()
----    --mod  <- var "Main"
----    --foo  <- a    @.  "foo"
----    --b    <- foo  @$$ [a]
----    --bar  <- mod  @.  "bar"
----    --c    <- bar  @$$ [b]
----    --plus <- mod  @.  "plus"
----    --out  <- plus @$$ [c, a]
--    return ()

main = do
    print f
    --let m = fck & view body
    --    g = fck & view fgraph
    --    Just (Ref p1) = Map.index "foo" m
    --    r = g ^. reg
    --    e = unsafeGet p1 r

    --print fck
    --print e
    print "end"


    --c = mempty :: Hetero' Vector

    --(c', p') = append2 ('a') c -- :: (Hetero' Vector, Ptr Int Char)
    --c'' = prepend_ 'o' c'

    --main = do
    --    print "end"
    --    print (c', p')
    --    print c''
    --    print $ uncheckedIndex (Ptr 0 :: Ptr Int Int) c'

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

----data Arg h = Arg { _label :: Maybe Name, _val :: HExpr h }


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


--g2 :: (ConvertibleM Expr a, Monad m, RefBuilder a m) => m (Ref m a)
--g2 = do
--    a <- ref_ $ var2 "a"
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





