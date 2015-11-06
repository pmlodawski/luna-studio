{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE DeriveAnyClass            #-}

module Data.Container.ST where


import Prologue hiding (index, Indexable)

import           Data.Container.Class
import           Data.Container.Opts  (Query(..), ModsOf, ParamsOf)
import qualified Data.Container.Opts  as M
import           Data.Container.Proxy
import           Data.Layer



import Data.STRef (STRef)


-------------------
-- === STRef === --
-------------------

--data STRef style a = STRef !style !a deriving (Show, Functor, Foldable, Traversable, Default, Monoid)

--type instance IndexOf      (STRef s a) = IndexOf (ContainerOf a)
--type instance ContainerOf  (STRef s a) = STRef s a
--type instance DataStoreOf  (STRef s a) = ContainerOf a

--instance      HasContainer (STRef s a) where container     = id
--instance      IsContainer  (STRef s a) where fromContainer = id

--type instance Unlayered (STRef s a) = a
--instance      Layered   (STRef s a) where layered = lens (\(STRef _ a) -> a) (\(STRef s _) a -> STRef s a)

--instance      (IsContainer a, FromList (ContainerOf a), Default s) 
--           => FromList  (STRef s a) where fromList = STRef def . fromContainer . fromList
--type instance Item      (STRef s a) = Item (ContainerOf a)

--style :: Lens' (STRef s a) s
--style = lens (\(STRef s _) -> s) (\(STRef _ a) s -> STRef s a)



--newSTRef :: a -> ST s (STRef s a)