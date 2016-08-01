module Data.IntConvertibleMap.Strict (
    IntConvertibleMap,
    toList, fromList,
    empty, null, size,
    singleton, insert, insertWith, insertWithKey,
    lookup, findWithDefault,
    delete, member, notMember,
    union, map, foldrWithKey
  ) where


import            Flowbox.Prelude     (Maybe(..), Int, Bool, ($), (.), fmap)
import            Data.Function       (on)
import            Data.Proxy

import            Data.IntMap.Strict  (IntMap)
import qualified  Data.IntMap.Strict  as IntMap
import qualified  Data.Maps           as GM
import            Data.Packable



data IntConvertibleMap k v = IntConvertibleMap { proxy :: (Proxy k), toIntMap :: (IntMap v) }


instance (Packable k Int) => GM.GenMap (IntConvertibleMap k v) k v where
    lookup          = lookup
    insertWithKey   = insertWithKey
    foldrWithKey    = foldrWithKey
    delete          = delete
    -- size            = size
    -- null            = null
    -- union           = union

    insert          = insert
    insertWith      = insertWith
    member          = member
    notMember       = notMember
    findWithDefault = findWithDefault



toList :: (Pack Int k) => IntConvertibleMap k v -> [(k, v)]
toList = fmap packKey . IntMap.toList . toIntMap
  where packKey (k,v) = (pack k, v)


fromList :: (Unpack k Int) => [(k, v)] -> IntConvertibleMap k v
fromList = IntConvertibleMap (Proxy::Proxy k) . IntMap.fromList . fmap unpackKey
  where unpackKey (k, v) = (unpack k, v)


singleton :: (Unpack k Int) => k -> v -> IntConvertibleMap k v
singleton k = IntConvertibleMap (Proxy::Proxy k) . IntMap.singleton (unpack k)


empty :: (Unpack k Int) => IntConvertibleMap k v
empty = IntConvertibleMap (Proxy::Proxy k) $ IntMap.empty


lookup :: (Unpack k Int) => k -> IntConvertibleMap k v -> Maybe v
lookup k = IntMap.lookup (unpack k) . toIntMap


union :: (Unpack k Int) => IntConvertibleMap k v -> IntConvertibleMap k v -> IntConvertibleMap k v
union mp1 mp2 = IntConvertibleMap (Proxy::Proxy k) $ (IntMap.union `on` toIntMap) mp1 mp2


map :: (Unpack k Int) => (a -> b) -> IntConvertibleMap k a -> IntConvertibleMap k b
map f = IntConvertibleMap (Proxy::Proxy k) . IntMap.map f . toIntMap


insertWithKey :: (Packable k Int) => (k -> v -> v -> v) -> k -> v -> IntConvertibleMap k v -> IntConvertibleMap k v
insertWithKey f k a = IntConvertibleMap (Proxy::Proxy k) . IntMap.insertWithKey (f . pack) (unpack k) a . toIntMap


foldrWithKey :: (Pack Int k) => (k -> v -> a -> a) -> a -> IntConvertibleMap k v -> a
foldrWithKey f a = IntMap.foldrWithKey (f . pack) a . toIntMap


delete :: (Unpack k Int) => k -> IntConvertibleMap k v -> IntConvertibleMap k v
delete k = IntConvertibleMap (Proxy::Proxy k) . IntMap.delete (unpack k) . toIntMap


size :: IntConvertibleMap k v -> Int
size = IntMap.size . toIntMap


null :: IntConvertibleMap k v -> Bool
null = IntMap.null . toIntMap


insert :: (Unpack k Int) => k -> v -> IntConvertibleMap k v -> IntConvertibleMap k v
insert k v = IntConvertibleMap (Proxy::Proxy k) . IntMap.insert (unpack k) v . toIntMap


insertWith :: (Unpack k Int) => (v -> v -> v) -> k -> v -> IntConvertibleMap k v -> IntConvertibleMap k v
insertWith f k v = IntConvertibleMap (Proxy::Proxy k) . IntMap.insertWith f (unpack k) v . toIntMap


member :: (Unpack k Int) => k -> IntConvertibleMap k v -> Bool
member k = IntMap.member (unpack k) . toIntMap


notMember :: (Unpack k Int) => k -> IntConvertibleMap k v -> Bool
notMember k = IntMap.notMember (unpack k) . toIntMap


findWithDefault :: (Unpack k Int) => v -> k -> IntConvertibleMap k v -> v
findWithDefault d k = IntMap.findWithDefault d (unpack k) . toIntMap

