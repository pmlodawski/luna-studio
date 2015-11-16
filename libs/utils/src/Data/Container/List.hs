module Data.Container.List where

import Data.Container.Class

import qualified GHC.Exts                as GHC
import qualified Data.Vector             as V


class FromList       l where fromList         :: [Item l] -> l
                             default fromList :: (GHC.IsList l, GHC.Item l ~ Item l)
                                              => [Item l] -> l
                             fromList = GHC.fromList

class ToList         l where toList         :: l -> [Item l]
                             default toList :: (GHC.IsList l, GHC.Item l ~ Item l)
                                            => l -> [Item l]
                             toList = GHC.toList

--class FromListUnsafe l where fromListUnsafe :: [Item l] -> l
--class ToListUnsafe   l where toListUnsafe   :: l -> [Item l]


type IsList l = (FromList l, ToList l)


-- === Instances ===

--type instance Item (V.Vector a) = a
--instance FromList  (V.Vector a)
--instance ToList    (V.Vector a)
