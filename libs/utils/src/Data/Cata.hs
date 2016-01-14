{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Cata where

import Prologue      hiding (Repr,repr)
import Control.Lens
import Data.Reprx
import Control.Monad.Trans.Identity

class    Monad m => MuBuilder a m             t | t m -> a where buildMu :: a (Mu t) -> m (Mu t)
instance Monad m => MuBuilder a (IdentityT m) a            where buildMu = return . Mu
instance            MuBuilder a Identity      a            where buildMu = return . Mu

instance {-# OVERLAPPABLE #-} (MonadTrans t, MuBuilder a m k, Monad (t m)) => MuBuilder a (t m) k where
    buildMu = lift . buildMu

class     Monad m           => ToMuM a          m t | a -> t where toMuM :: a -> m (Mu t)
instance  Monad m           => ToMuM    (Mu t)  m t          where toMuM = return
instance (Monad m, (m ~ n)) => ToMuM (n (Mu t)) m t          where toMuM = id


-- === Catamorphisms ===

newtype Mu f = Mu (f (Mu f))

instance Rewrapped (Mu f) (Mu f')
instance Wrapped   (Mu f) where
    type Unwrapped (Mu f) = f (Mu f)
    _Wrapped' = iso (\(Mu a) -> a) Mu

--newtype MuH h f = MuH ( f (h (MuH h f)) )

type MendlerAlgebra f c = forall a. (a -> c) -> f a -> c
type Algebra        f a = f a -> a

--- utils ---

cata  :: Functor f                => (f c ->   c) -> Mu f ->   c
cataM :: (Traversable f, Monad m) => (f c -> m c) -> Mu f -> m c
cata  f =             f . fmap (cata  f) . unwrap
cataM f = join . fmap f . mapM (cataM f) . unwrap

mendlerCata :: MendlerAlgebra f c -> Mu f -> c
mendlerCata phi = phi (mendlerCata phi) . (unwrap)

-- | The same as Mendler-style catamorphism:
-- | muCata phi = mendlerCata (\f -> phi . fmap f)
muCata :: Functor f => Algebra f c -> Mu f -> c
muCata = cata


--- instances ---

deriving instance Show   (f (Mu f)) => Show   (Mu f)
instance          Repr s (f (Mu f)) => Repr s (Mu f) where repr = repr . unwrap
