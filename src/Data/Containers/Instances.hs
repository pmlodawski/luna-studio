{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Containers.Instances where

import           Flowbox.Prelude hiding (Indexable, index, Bounded)
import           Data.Containers.Class

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           Control.Monad.ST
import           Data.Typeable

-- TF instances
type instance ElementOf (IntMap a) = a
type instance ElementOf (Map  k a) = a

type instance IndexOf el (IntMap a) = Int
type instance IndexOf el (Map  k a) = k

type instance ElementByIx  idx (IntMap a) = a
type instance ElementByIx  idx (Map  k a) = a


-- vector instances

type instance ElementOf        (V.Vector a) = a
type instance ElementByIx  idx (V.Vector a) = a
type instance IndexOf      el  (V.Vector a) = Int

instance            Bounded            (V.Vector a) Int   where minBoundIdx _ = 0
                                                                maxBoundIdx v = size v - 1

instance            Measurable          (V.Vector a)       where size                    = fromIntegral . V.length
instance            Container           (V.Vector a) Int a where elems                   = V.toList
                                                                 indexes               v = [minIdx v .. maxIdx v]
instance (a ~ b) => Appendable'         (V.Vector a) Int b where append'             a v = (v', V.length v' - 1) where v' = V.snoc v a
instance (a ~ b) => Prependable         (V.Vector a) Int b where prepend'            a v = (v', 0)               where v' = V.cons a v


instance (a ~ b) => Indexable2 '[]        (V.Vector a) Int b where index2 _ idx v = (V.!)           v idx
instance (a ~ b) => Indexable2 '[Unsafe2] (V.Vector a) Int b where index2 _ idx v = (V.unsafeIndex) v idx

instance (a ~ b) => Insertable          (V.Vector a) Int b where insert          idx a v = (V.//)          v [(idx,a)]
                                                                 unsafeInsert    idx a v = (V.unsafeUpd)   v [(idx,a)]
instance (a ~ b) => Updatable           (V.Vector a) Int b where update          idx a v = unsafeInsert idx a v


instance Allocable   (V.Vector a)     where unsafeAlloc    idx   = runST $ V.unsafeFreeze =<< MV.unsafeNew idx
instance Growable    (V.Vector a)     where unsafeGrow     idx v = runST $ V.unsafeFreeze =<< flip MV.unsafeGrow idx =<< V.unsafeThaw v
instance Growable'   (V.Vector a) Int where unsafeGrow'    idx v = (unsafeGrow idx v, [size v .. size v + idx - 1])
instance Expandable  (V.Vector a)

  --instance Erasable    (V.Vector a) Int
  --instance Erasable'   (V.Vector a) Int a where unsafeErase' idx v = (insert idx (error $ "uninitialised element @ " <> show idx) v, index idx v)

-- list instances

--type instance ElementOf        [a] = a
--type instance ElementByIx  idx [a] = a
--type instance IndexOf       el [a] = Int


--instance            Measurable   [b]       where size               = fromIntegral . length
--instance            Container    [b] Int b where elems              = id
--                                                 indexes          l = [0 .. size l -1]
--instance (a ~ b) => Appendable'  [b] Int a where append'        a l = case l of []     -> ([a], 0)
--                                                                                (x:xs) -> (x:l', idx' + 1) where
--                                                                                    (l', idx') = append' a xs
--instance (a ~ b) => Prependable  [b] Int a where prepend'       a l = (a:l, 0)
--instance (a ~ b) => Indexable2 opts [b] Int a where index opts idx l = case (idx, l) of
--                                                                          (0, (x:_))  -> x
--                                                                          (_, (_:xs)) -> index (idx - 1) xs
--                                                                          _           -> error "index out of range"

--instance (a ~ b) => Updatable    [b] Int a where update     idx a l = case (idx, l) of
--                                                                          (0, (x:xs)) -> a : xs
--                                                                          (_, (x:xs)) -> x : update (idx - 1) a xs
--                                                                          _           -> error "index out of range"




---- missing instances

instance Default (V.Vector a) where def = mempty
