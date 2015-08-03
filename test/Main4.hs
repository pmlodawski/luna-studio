
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

import Flowbox.Prelude hiding (simple, empty, Indexable, Simple, cons, lookup, index)
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
--data Node = Node


-- === Expr ===
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


data Label l a = Label l a



newtype Ptr  i a = Ptr  i


class    IsPtr2 i p a | p -> i where mkPtr2 :: i -> p a
class    IsPtr p i | p -> i where mkPtr :: i -> p a
instance IsPtr (Ptr i) i    where mkPtr = Ptr


instance (Typeable a, Show i) => Show (Ptr i a) where
    show (Ptr i) = "Ptr " <> show i <> " (" <> show (typeOf (undefined :: a)) <> ")"

class    HasPtr t         i a | t -> i a where ptr :: Lens' t (Ptr i a)
instance HasPtr (Ptr i a) i a            where ptr = id


instance Convertible i (Ptr i a) where convert = Ptr
instance Convertible (Ptr i a) i where convert (Ptr i) = i

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
        App      base args -> base : fmap (view arg) args
        _                  -> []







--- === [TOEXTRACT] Instances ===
instance Default (Vector a) where def = mempty





--- === Hidden ===

type NodeVal a = (Show a, Typeable a, Repr a)

data Hidden where
    Hidden :: NodeVal a => a -> Hidden



castableLens :: IsoCastable a b => Iso' a b
castableLens = iso cast cast

convertibleLens :: IsoConvertible a b => Iso' a b
convertibleLens = iso convert convert

-- instances

instance Show Hidden where show (Hidden a) = show a
instance Repr Hidden where repr (Hidden a) = repr a

instance {-# OVERLAPPABLE #-} Castable Hidden a where
    cast (Hidden a) = unsafeCoerce a

instance {-# OVERLAPPABLE #-} NodeVal a => Castable a Hidden where
    cast = Hidden



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

type instance IndexOf el [a]                    = Int
type instance IndexOf el (Vector a)             = Int
type instance IndexOf el (HeteroContainer cont) = Ptr (Index cont) el

type instance ElementByIdx idx [a]                    = a
type instance ElementByIdx idx (Vector a)             = a
type instance ElementByIdx idx (HeteroContainer cont) = IdxType idx

type instance IdxType (Ptr i a) = a



--- === Voids ===

data Void = Void deriving (Show)
class Void1 a
instance Void1 a
-- where



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



--- === HasContainer ===

class HasContainer a cont | a -> cont where
    container :: Lens' a cont

instance HasContainer [a]           [a]           where container = id
instance HasContainer (Vector a)    (Vector a)    where container = id



--- === Hetero Containers ===

-- TODO: make it PolyKinded
--type family Hetero (ctx :: * -> Constraint) (cont :: * -> *) :: *

type Hetero ctx cont = HeteroContainer (cont (Unified ctx))
type Hetero'    cont = Hetero Void1 cont

newtype HeteroContainer cont = HeteroContainer { _cont :: cont } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''HeteroContainer

-- instances

--type instance Hetero ctx cont = HeteroContainer (cont (Unified ctx))
instance HasContainer (HeteroContainer cont) cont where container = cont


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

--append2 :: (Appendable elems, Indexable elems, Castable a (ElementOf elems), IsPtr2 (Index elems) ptr (a ptr))
--       => a -> HContainer elems -> (HContainer elems, ptr a)
--append2 a g = (g', mkPtr . lastIdx $ g' ^. elems) where
--    g' = g & elems %~ (flip append $ cast a)

insertx :: (Appendable elems, LastIdx elems, Castable a (ElementOf elems), IsPtr2 (Index elems) p a)
        => a -> HContainer elems -> (HContainer elems, p a)
insertx a g = (g', mkPtr2 . lastIdx $ g' ^. elems) where
    g' = g & elems %~ (flip append $ cast a)

insert2 :: (Appendable elems, LastIdx elems, IsPtr2 (Index elems) p el, el ~ ElementOf elems)
        => el -> HContainer elems -> (HContainer elems, p el)
insert2 a g = (g', mkPtr2 . lastIdx $ g' ^. elems) where
    g' = g & elems %~ (flip append a)

type instance Index     (HContainer elems) = Index     elems
type instance ElementOf (HContainer elems) = ElementOf elems



--instance Appendable2 (HContainer elems) where
--    append2 el c = (c', Ptr . lastIdx $ c' ^. elems) where
--        c' = c & elems %~ (flip append el)

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

--data Value a = Val  a
--             | BVal -- binary value



--- === Ctx ===

data Ctx a = Pure
           | IO       (Maybe a)
           | Unknown  (Maybe a)
           | Ctx Name (Maybe a)
           deriving (Show)

--data Val a = Val { _ctx   :: Ctx a
--                 , _trans :: [Ctx a]
--                 , _val   :: a
--                 }


--- === Builder ===


data Typex = Typex deriving (Show) -- fixme
data Tp a  = Tp Typex a

newtype BasePtr  a = BasePtr  { __ptr  :: Ptr Int a        } deriving (Show)
newtype TPtr     a = TPtr     { __tptr :: Ptr Int (Tp a)   } deriving (Show)
--newtype NodePtr  a = NodePtr  { __nptr :: Ptr Int (Node a) } deriving (Show)
--newtype NodePtr  (m a) = NodePtr  { __nptr :: Ptr Int (Node a) } deriving (Show)

data NodePtr t where NodePtr :: (Ptr Int (Node a h)) -> NodePtr (a h)

--data HPtr m t where HPtr :: (Ptr Int (m a h)) -> HPtr (a h)

deriving instance (t ~ a (h :: * -> *), Typeable a, Typeable h) => Show (NodePtr t)

--instance Convertible (Ptr i (Node a h))

getPtr :: NodePtr (a h) -> Ptr Int (Node a h)
getPtr (NodePtr p) = p

setPtr :: NodePtr (a h) -> Ptr Int (Node a h) -> NodePtr (a h)
setPtr _ p = NodePtr p

data Node a (h :: * -> *) = Node Typex (Ctx (a h)) [Ctx (a h)] (a h) deriving (Show)

data Node2 a = Node2 Typex (Ctx a) [Ctx a] a deriving (Show)

--instance ConvertibleM m (Node m) where
--    convertM a = Node Typex Pure [] a

class ASTConvertible m n where
    convertAST :: m h -> n (h :: * -> *)

instance {-# OVERLAPPABLE #-} ASTConvertible a a where convertAST = id

instance ASTConvertible Expr (Node Expr) where
    convertAST = Node Typex (Unknown Nothing) []



type family PtrOf (a :: (* -> *) -> *) :: * -> *
type instance PtrOf Expr     = BasePtr
type instance PtrOf (Node a) = NodePtr

--type family PtrOf2 (a :: (* -> *) -> *) :: *
--type instance PtrOf2 (Node a) = NodePtr (a NodePtr)

type family PtrOf3 a
type instance PtrOf3 (Node a h) = NodePtr (a h)
type instance PtrOf3 (Expr h) = BasePtr (Expr h)


type family ValOf (a :: (* -> *) -> *) :: *
type instance ValOf (Node a) = (a NodePtr)

type family ValOf3 a :: *
type instance ValOf3 (NodePtr (a h)) = (Node a h)

--type family PtrOf2 (a :: (* -> *) -> *) b :: *
--type instance PtrOf2 (Node a) = NodePtr a

type family ASTType (a :: (* -> *) -> *) :: (* -> *) -> *
type instance ASTType (Node a) = a
type instance ASTType Expr = Expr

type family ASTType2 (a :: *) :: (* -> *) -> *
type instance ASTType2 (Node a h) = a
type instance ASTType2 (Expr h) = Expr


type family ChildType (h :: * -> *) (ast :: (* -> *) -> *) :: (* -> *) -> *
type instance ChildType NodePtr Expr = Node Expr
type instance ChildType BasePtr Expr = Expr

makeLenses ''BasePtr
makeLenses ''TPtr
--makeLenses ''NodePtr

instance HasPtr (BasePtr a)     Int a          where ptr = _ptr
instance HasPtr (TPtr a)        Int (Tp a)     where ptr = _tptr
instance HasPtr (NodePtr (a h)) Int (Node a h) where ptr = lens getPtr setPtr

instance IsPtr BasePtr  Int where mkPtr = BasePtr . Ptr
instance IsPtr TPtr     Int where mkPtr = TPtr    . Ptr

instance IsPtr2 Int BasePtr  a                where mkPtr2 = BasePtr . Ptr
instance IsPtr2 Int TPtr     a                where mkPtr2 = TPtr    . Ptr
instance IsPtr2 Int NodePtr (a (h :: * -> *)) where mkPtr2 = NodePtr . Ptr

--Expr h = ... (h Expr)



--instance IsPtr (Ptr i) i    where mkPtr = Ptr

--

newtype HeteroGraph   = HeteroGraph { __hetReg :: Hetero' Vector } deriving (Show, Default)
newtype HomoGraph   a = HomoGraph   { __homReg :: Vector a       } deriving (Show, Default)

makeLenses ''HeteroGraph
makeLenses ''HomoGraph

instance HasContainer HeteroGraph   (Hetero' Vector) where container = _hetReg
instance HasContainer (HomoGraph a) (Vector a)       where container = _homReg



type GraphBuilder g m = StateT (BldrState g) m

data BldrState g = BldrState { _orphans :: [Int]
                             , _graph   :: g
                             }

makeClassy ''BldrState


class Monad m => MonadBldrState g m | m -> g where
    getBldrState :: m (BldrState g)
    putBldrState :: (BldrState g) -> m ()


instance Default g => Default (BldrState g) where
    def = BldrState def def


instance Monad m => MonadBldrState g (GraphBuilder g m) where
    getBldrState = get
    putBldrState = put


instance (Monad m, Convertible idx (RefVal a), HasContainer g cont, Appendable2 (Val a) idx cont)
      => RefBuilder (GraphBuilder g m) a where
    mkRef a = fmap (Ref . convert) . withGraph . mapOver container $ append2 a


mkRef3x :: (Convertible idx (h (a h)), HasContainer g cont, Appendable2 (ChildOf h a) idx cont, MonadBldrState g m)
        => ChildOf h a -> m (Ref3 h a)
mkRef3x a = fmap (Ref3 . convert) . withGraph . mapOver container $ append2 a


--instance (Monad m, Convertible idx (RefVal a), HasContainer g cont, Appendable2 (Val a) idx cont)
--      => RefBuilder3 (GraphBuilder g m) a where
--    mkRef a = fmap (Ref . convert) . withGraph . mapOver container $ append2 a


--class Monad m => RefBuilder3 m h a where
--    mkRef3 :: ChildOf h a -> m (Ref3 h a)


instance (Convertible idx (h (a h)), HasContainer g cont, Appendable2 (ChildOf h a) idx cont, Monad m)
      => RefBuilder3 (GraphBuilder g m) h a where
    mkRef3 a = fmap (Ref3 . convert) . withGraph . mapOver container $ append2 a


var :: (ASTConvertible Expr (ChildType h a), RefBuilder3 m h a) => Name -> m (Ref3 h a)
var = mkRef3 . convertAST . Var

xx :: _ => m (Ref3 NodePtr Expr)
xx = var "foo"

--instance (Monad m, Convertible idx (RefVal a), HasContainer g cont, Appendable2 (Val a) idx cont)
--      => RefBuilder2 (GraphBuilder g m) a where

--mkRef2x :: (Convertible idx (PtrOf3 a), HasContainer g cont, Appendable2 el idx cont, MonadBldrState g f)
--        => el -> f (Ref2 a)
--mkRef2x a = fmap (Ref2 . convert) . withGraph . mapOver container $ append2 a

--x :: PtrOf3 a ~ Expr h => Ref2 a
--x = Ref2 $ Var "a"
--var :: (ASTConvertible Expr a, RefBuilder m a) => Name -> m (Ref a)
--var2 = mkRef2x . convertAST . Var



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

        --withOrphans :: MonadBldrState m => ([Int] -> (a, [Int])) -> m a
        --withOrphans f = do
        --    s <- getBldrState
        --    let orphLens    = bldrState . orphans
        --        (a, norphs) = f (s ^. orphLens)
        --    putBldrState $ s & orphLens .~ norphs
        --    return a

        --popOrphan :: MonadBldrState m => m (Maybe Int)
        --popOrphan = withOrphans $ \case
        --    []         -> (Nothing, [])
        --    (id : ids) -> (Just id, ids)



---- === Ref ===

--RefVal (Node Expr)
--PtrOf (Node Expr) (Val (Node Expr))
--IPtr (Node Expr IPtr)


--RefVal (Node Expr)
--PtrOf (Node Expr) (Val (Node Expr))
--NodePtr (Node Expr NodePtr)

--Ref (Node Expr)
--RefVal (Node Expr)
--IPtr (Val (Node Expr))
--IPtr (Node Expr NodePtr)

--type    Val         a = a (PtrOf a)
type    RefVal      a = Ptr Int (Val a)
type    Val         a = a (PtrOf a)
--type    RefVal      a = PtrOf3 a
newtype Ref         a = Ref { fromRef :: RefVal a }

newtype Ref2        a = Ref2 { fromRef2 :: RefVal2 a }
type    RefVal2     a = PtrOf3 a

type    Val2        a = a (PtrOf a)

newtype Ref3 h a = Ref3 (h (a h))

--newtype Ref4 a = Ref4 (h (a h))


--xx :: _ => m (Ref3 NodePtr Expr)
--xx = var "foo"


--Ref4 (Node Expr)

        --newtype RefCons m h a = RefCons { runRefCons :: m (Ref h a) }
        --newtype RefCons2 m a  = RefCons2 { runRefCons2 :: m (Ref a) }

class ToMRef t m a | t -> a where
    toMRef :: t -> m (Ref a)

instance                             (Monad m) => ToMRef    (Ref a)  m a where toMRef = return
instance {-# OVERLAPPABLE #-} (m ~ n, Monad m) => ToMRef (m (Ref a)) n a where toMRef = id

        --deriving instance Show (h (a h)) => Show (Ref h a)

-- === RefBuilder ===

class Monad m => RefBuilder2 m a where
    mkRef2 :: a h -> m (h (ASTType a h))

class Monad m => RefBuilder m a where
    mkRef :: Val a -> m (Ref a)

class Monad m => RefBuilder3 m h a where
    mkRef3 :: ChildOf h a -> m (Ref3 h a)

type ChildOf h a = ChildType h a h


--mkRef3x :: (Convertible idx (PtrOf3 a), HasContainer g cont, Appendable2 el idx cont, MonadBldrState g f)
--        => el -> f (Ref2 a)


        --class Monad m => RefHandler m h a | m -> h a where
        --    registerRef :: Name -> Ref h a -> m ()

        --type ASTBuilder m h a = (RefBuilder m h a, RefHandler m h a)

        ----


        --mkCons2 :: RefBuilder m a => a (PtrOf a) -> RefCons2 m a
        --mkCons2 = RefCons2 . mkRef


--var :: (ASTConvertible Expr a, RefBuilder m a) => Name -> m (Ref a)
--var = mkRef . convertAST . Var

        ----test :: RefBuilder

        ----dorobic SeqBuidler i pozenic z grafami - seq builder moze budowac body funkcji i laczyc je z kontekstami!

        --cons :: RefBuilder m h Expr => Name -> RefCons m h Expr
        --cons = mkCons . Cons

        --ref :: (RefHandler m h a, ToMRef t m h a) => Name -> t -> m (Ref h a)
        --ref name t = do
        --    ref <- toMRef t
        --    registerRef name ref
        --    return ref

--mrefRaw :: (ToMRef t m a, Functor m) => t -> m (RefVal a)
--mrefRaw = fmap fromRef . toMRef

--accessor :: (RefBuilder m Expr, ToMRef t m Expr) => Name -> t -> m (Ref Expr)
--accessor name el = mkRef . convertAST . Accessor name =<< mrefRaw el


        --match :: (ToMRef t1 m h Expr, ToMRef t2 m h Expr, RefBuilder m h Expr, Monad m)
        --      => t1 -> t2 -> RefCons m h Expr
        --match a b = RefCons $ do
        --    ra <- mrefRaw a
        --    rb <- mrefRaw b
        --    mkRef $ Match ra rb



        ------ === RefBuilder ===


        ----def foo [x,y] : ...

        --data Function body   = Function { -- _args   :: [Pattern h]
        --                                  _body   :: body
        --                                } deriving (Show)

        --type NodeFunction ptr = Function (GFBody ptr)

        --type GFBody ptr = Map String (Ref ptr Expr)

        --instance Default body => Default (Function body) where def = Function def

        ----deriving instance Show (Function body h)

        --makeLenses ''Function
        ------ === Examples ===


runGraphBuilder :: (Monad m, Default g) => GraphBuilder g m a -> m g
runGraphBuilder gb = view graph <$> execStateT gb def

--runHomoGraphBuilder :: Monad m => GraphBuilder HomoGraph m a -> m HomoGraph
--runHomoGraphBuilder gb = view graph <$> execStateT gb def

        --runFunctionBuilder :: (Default body, Monad m) => FunctionBuilder body m a -> m (a, Function body)
        --runFunctionBuilder fb = runStateT fb def

        --type FunctionBuilder body m = StateT (Function body) m

        --class MonadFunctionBuilder body m | m -> body where
        --    getFunction :: m (Function body)
        --    putFunction :: Function body -> m ()

        --instance Monad m => MonadFunctionBuilder body (FunctionBuilder body m) where
        --    getFunction = get
        --    putFunction = put


        ----class Monad m => RefHandler m h a | m -> h a where
        ----    registerRef :: Name -> Ref h a -> m ()

        --instance {-# OVERLAPPABLE #-} (MonadTrans t, RefHandler m h a, Monad (t m)) => RefHandler (t m) h a where
        --    registerRef name ref = lift $ registerRef name ref

        --instance Monad m => RefHandler (FunctionBuilder (GFBody h) m) h Expr where
        --    registerRef name ref = do
        --        func <- getFunction
        --        putFunction $ func & body %~ Map.insert name ref


        ----foo :: _
        ----buildFunction desc = f & fgraph .~ g where
        ----    (g,f) = runIdentity $ runFunctionBuilder $ runGraphBuilder desc

--tst :: HomoGraph (Node Expr NodePtr)
--tst = runIdentity $ runGraphBuilder g1
        ----tst :: (Graph, Function (GFBody BasePtr))
        ----fck :: Function (GFBody BasePtr)
        ----fck = buildFunction g1

--g1 :: _ => m (Ref (Node Expr))

--g1 :: HomoGraph (Node Expr NodePtr)
--g1 = runIdentity $ runGraphBuilder $ do
--    a <- var "a"
--    --a    <- ref "foo" $ var "a"
--    --b    <- ref "bar" $ accessor "foo" (var "b")
--    --c    <- ref "baz" $ match (var "a") (var "b")
--    return ()
--    --mod  <- var "Main"
--    --foo  <- a    @.  "foo"
--    --b    <- foo  @$$ [a]
--    --bar  <- mod  @.  "bar"
--    --c    <- bar  @$$ [b]
--    --plus <- mod  @.  "plus"
--    --out  <- plus @$$ [c, a]
--    --return ()

main = do
    --print g1
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





