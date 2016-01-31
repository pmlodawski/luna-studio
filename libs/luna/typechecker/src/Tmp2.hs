{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Tmp2 where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound, (#), assert, Index)

import Data.Record hiding (Layout)


import Luna.Syntax.AST.Term2 hiding (Lit, Val, Thunk, Expr, Draft)
import qualified Luna.Syntax.AST.Term2 as Term
import Luna.Syntax.Model.Layer.Labeled


import Data.Layer.Cover
import Data.Coat
import Data.Construction

import Control.Monad.Identity
import Control.Monad.State
import Data.Container

import           Luna.Syntax.Model.Graph (Graph, GraphBuilder, MonadGraphBuilder, nodes, edges)
import qualified Luna.Syntax.Model.Graph as Graph

import Data.Construction

import Control.Monad.Reader

import qualified Luna.Syntax.Model.Builder.Type as Type
import           Luna.Syntax.Model.Builder.Type (MonadTypeBuilder, TypeBuilder, TypeBuilderT)

import Luna.Syntax.Model.Builder.Self (MonadSelfBuilder, SelfBuilderT, self, setSelf)
import qualified Luna.Syntax.Model.Builder.Self as Self

import Type.Bool

import Luna.Syntax.AST.Layout (Static, Dynamic)



newtype Tagged t a = Tagged a deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

makeWrapped ''Tagged

----------------------------------
-- === Network Declarations === --
----------------------------------

data Network (ls :: [*]) = Network

data Ptr  i a = Ptr i                deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data Ref    a = Ref  (Ptr Int a)     deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data Link   a = Link (Ref a) (Ref a) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data Node   a = Node a               deriving (Show, Eq, Ord, Functor, Traversable, Foldable)


-- === Instances === --

makeWrapped ''Node
type instance Unlayered (Node a) = a
instance      Layered   (Node a)

instance Monad m => LayerConstructor m (Node a) where
    constructLayer = return ∘ Node ; {-# INLINE constructLayer #-}


type instance Unlayered (Ref a) = a
instance (MonadGraphBuilder n e m, Castable a n) => LayerConstructor m (Ref (Node a)) where
    constructLayer n = Ref ∘ Ptr <$> Graph.modify (nodes $ swap ∘ ixed add (cast ast)) where
        ast = unwrap' n :: a


--instance (a ~ Int, MonadGraphBuilder n e m, Castable t n)
--      => Constructor m (Ref (Targetting Node t) a) where
--    construct ast = Ref <$> Graph.modify (nodes $ swap ∘ ixed add (cast ast))

--Graph ('[Note] :< Raw) Int

---------------------------
-- === Network Terms === --
---------------------------

-- === Definitions === --

type family TermWrapper (a :: *) :: * -> [*] -> *

data    Raw      (ls :: [*]) = Raw Data                                deriving (Show)

newtype Lit   rt (ls :: [*]) = Lit   (Term (Network ls) Term.Lit   rt) deriving (Show)
newtype Val   rt (ls :: [*]) = Val   (Term (Network ls) Term.Val   rt) deriving (Show)
newtype Thunk rt (ls :: [*]) = Thunk (Term (Network ls) Term.Thunk rt) deriving (Show)
newtype Expr  rt (ls :: [*]) = Expr  (Term (Network ls) Term.Expr  rt) deriving (Show)
newtype Draft rt (ls :: [*]) = Draft (Term (Network ls) Term.Draft rt) deriving (Show)


-- === Instances === --

-- Wrappers

makeWrapped ''Lit
makeWrapped ''Val
makeWrapped ''Thunk
makeWrapped ''Expr
makeWrapped ''Draft

-- Term bindings

type instance TermWrapper Term.Lit   = Lit
type instance TermWrapper Term.Val   = Val
type instance TermWrapper Term.Thunk = Thunk
type instance TermWrapper Term.Expr  = Expr
type instance TermWrapper Term.Draft = Draft

-- Records

type instance RecordOf (Lit   rt ls) = RecordOf (Unwrapped (Lit   rt ls))
type instance RecordOf (Val   rt ls) = RecordOf (Unwrapped (Val   rt ls))
type instance RecordOf (Thunk rt ls) = RecordOf (Unwrapped (Thunk rt ls))
type instance RecordOf (Expr  rt ls) = RecordOf (Unwrapped (Expr  rt ls))
type instance RecordOf (Draft rt ls) = RecordOf (Unwrapped (Draft rt ls))

instance IsRecord (Lit   rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Val   rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Thunk rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Expr  rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Draft rt ls) where asRecord = wrapped' ∘ asRecord

-- Conversions

instance Castable (Draft rt ls) (Raw ls) where cast = Raw ∘ cast ∘ unwrap' ; {-# INLINE cast #-}



--------------------
-- === Layers === --
--------------------

-- === Definition === --

data Layer t a = Layer (LayerData (Layer t a)) a
type family AttachedData d a


-- === Utils === --

type family LayerData l where LayerData (Layer t a) = Tagged t (AttachedData t (Uncovered a))


-- === Instances === --

deriving instance (Show (AttachedData t (Uncovered a)), Show a) => Show (Layer t a)

type instance Unlayered (Layer t a) = a
instance      Layered (Layer t a) where
    layered = lens (\(Layer _ a) -> a) (\(Layer d _) a -> Layer d a) ; {-# INLINE layered #-}

instance (Maker m (LayerData (Layer t a)), Functor m)
      => LayerConstructor m (Layer t a) where
    constructLayer a = flip Layer a <$> make ; {-# INLINE constructLayer #-}

instance (Castable a a', Castable (LayerData (Layer t a)) (LayerData (Layer t' a')))
      => Castable (Layer t a) (Layer t' a') where
    cast (Layer d a) = Layer (cast d) (cast a) ; {-# INLINE cast #-}

--------------------------
-- === Basic layers === --
--------------------------

-- === Note === --

data Note = Note deriving (Show)
type instance AttachedData Note t = String

instance Monad m => Maker m (Tagged Note String) where make = return $ Tagged $ ""


--------------------
-- === Shell === ---
--------------------

data (layers :: [*]) :< (a :: [*] -> *) = Shell (ShellStrcture layers (a layers))

type family ShellStrcture ls a where 
    ShellStrcture '[]       a = Cover a
    ShellStrcture (l ': ls) a = Layer l (ShellStrcture ls a)


-- === Instances === --

deriving instance Show (Unwrapped (ls :< a)) => Show (ls :< a)

makeWrapped ''(:<)
type instance Unlayered (ls :< a) = Unwrapped (ls :< a)
instance      Layered   (ls :< a)

instance Monad m => LayerConstructor m (ls :< a) where
    constructLayer = return ∘ wrap' ; {-# INLINE constructLayer #-}

instance Castable (Unwrapped (ls :< a)) (Unwrapped (ls' :< a')) => Castable (ls :< a) (ls' :< a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}



------------------------------------
-- === Network Implementation === --
------------------------------------

type instance Layout (Network ls) term rt = Link (ls :< TermWrapper term rt)



-------------------------------
-- === Node constructors === --
-------------------------------

star :: Draft Static '[Note]
star = cons Star



-------------------------------------------------------------------------------------------------------------------------------------------------
-- TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST --
-------------------------------------------------------------------------------------------------------------------------------------------------


type NetGraph = Graph ('[Note] :< Raw) Int

buildNetwork  = runIdentity ∘ buildNetworkM
buildNetworkM = rebuildNetworkM def
rebuildNetworkM (net :: NetGraph) = -- flip Self.evalT (undefined :: Netref Node (Static Draft))
                                   -- ∘ flip Type.evalT (Nothing :: Maybe (Netref Node (Static Draft)))
                                   -- ∘ constrainType Node (Proxy :: Proxy (Netref Node n))
                                   -- ∘ constrainType Edge (Proxy :: Proxy (Netref Edge e))
                                  flip Graph.runT net
-- {-# INLINE   buildNetworkM #-}
-- {-# INLINE rebuildNetworkM #-}




foo = runIdentity
    $ rebuildNetworkM def
    $ do
    constructCover star

mytest :: IO ()
mytest = do 
    let s1 = cons Star :: Draft Static '[Note]

        --s1' = Shell (Layer "oh" (Cover s1)) :: '[Note] :< (Term (Network '[Note]) Draft Static)

        u1 = cons $ Unify (Link (Ref (Ptr 0)) (Ref (Ptr 1)) :: Link ('[Note] :< Draft Static)) 
                          (Link (Ref (Ptr 0)) (Ref (Ptr 1)) :: Link ('[Note] :< Draft Static)) 
           :: Draft Static '[Note]
        --u1 = cons $ Unify (Link (Ptr 0) (Ptr 1) :: Link ('[Note] :< (Draft Static))) (Link (Ptr 0) (Ptr 1) :: Link ('[Note] :< (Draft Static))) :: Draft Static

        (s2, g) = foo :: (Ref $ Node ('[Note] :< Draft Static), NetGraph)
    print s2
    print g

    --print $ caseTest s $ do
        --match $ \ -> "oh"
        --match $ \ANY -> "oh"

    return ()

--Ref $ Link $ '[Note] :> Draft Static --> read
--      Link $ '[Note] :> Draft Static --> target
--Ref $ Node $ '[Note] :> Draft Static --> read
--      Node $ '[Note] :> Draft Static --> uncover
--                        Draft Static '[Note]

---

--Ref $ Node '[Note] Static Draft --> read
--      Node '[Note] Static Draft --> uncover
--      Term (Network '[Note]) Static Draft

--Ref $ Link $ Node '[Note] Static Draft --> read
--      Link $ Node '[Note] Static Draft --> target
--       Ref $ Node '[Note] Static Draft --> read
--             Node '[Note] Static Draft --> uncover
--             Term (Network '[Note]) Static Draft


--Ref $ Link $ Node ('[Note] :> Draft Static) --> read
--      Link $ Node ('[Note] :> Draft Static) --> target
--       Ref $ Node ('[Note] :> Draft Static) --> read
--             Node ('[Note] :> Draft Static) --> uncover
--                              Draft Static '[Note]

--Ref $ Link $ '[Note] :> Draft Static --> read
--      Link $ '[Note] :> Draft Static --> target
--       Ref $ '[Note] :> Draft Static --> read
--             '[Note] :> Draft Static --> uncover
--                        Draft Static '[Note]
