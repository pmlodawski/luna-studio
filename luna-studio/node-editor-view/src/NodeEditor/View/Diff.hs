module NodeEditor.View.Diff where

import           Common.Prelude            hiding (get)
import           Control.Arrow             ((&&&))
import           Control.Monad.Trans.State
import           Data.Convert              (Convertible)
import           Data.Hashable             (Hashable)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set


type DiffT a = StateT (a, a)

runDiffT :: Monad m => DiffT a m r -> a -> a -> m r
runDiffT fun = curry $ evalStateT fun

diff :: Monad m => DiffT b m r -> Getting b a b -> DiffT a m r
diff fun getting = do
    (old, new) <- get
    lift $ runDiffT fun (old ^. getting) (new ^. getting)

diffApply :: (Eq a, Monad m) => (a -> DiffT a m ()) -> DiffT a m ()
diffApply apply = do
    (old, new) <- get
    when (new /= old) $ apply new

diffConvert :: (Convertible a b, Monad m) => DiffT b m r -> DiffT a m r
diffConvert = flip diff $ to convert

diffHashMap
    :: forall k v m. (Eq k, Eq v, Hashable k, Monad m)
    => (DiffT v m ())
    -> (v -> DiffT (HashMap k v) m ())
    -> (v -> DiffT (HashMap k v) m ())
    -> DiffT (HashMap k v) m ()
diffHashMap = diffContainer difference intersectWith where
    difference    = HashMap.elems .: HashMap.difference
    intersectWith = HashMap.elems .: HashMap.intersectionWith (,)

diffMap
    :: forall k v m. (Eq k, Eq v, Monad m, Ord k)
    => (DiffT v m ())
    -> (v -> DiffT (Map k v) m ())
    -> (v -> DiffT (Map k v) m ())
    -> DiffT (Map k v) m ()
diffMap = diffContainer difference intersectWith where
    difference    = Map.elems .: Map.difference
    intersectWith = Map.elems .: Map.intersectionWith (,)

diffMapWithKey
    :: forall k v m. (Eq k, Eq v, Monad m, Ord k)
    => (DiffT (k,v) m ())
    -> ((k, v) -> DiffT (Map k v) m ())
    -> ((k, v) -> DiffT (Map k v) m ())
    -> DiffT (Map k v) m ()
diffMapWithKey = diffContainer difference intersectWith where
    difference = Map.toList .: Map.difference
    intersectWith
        = Map.elems .: Map.intersectionWithKey (\k v1 v2 -> ((k,v1), (k,v2)))

diffSet
    :: forall v m. (Eq v, Monad m, Ord v)
    => (DiffT v m ())
    -> (v -> DiffT (Set v) m ())
    -> (v -> DiffT (Set v) m ())
    -> DiffT (Set v) m ()
diffSet = diffContainer difference intersectWith where
    difference    = Set.elems .: Set.difference
    intersectWith = fmap (id &&& id) . Set.elems .: Set.intersection

diffContainer
    :: (Monad m)
    => (a -> a -> [v])
    -> (a -> a -> [(v,v)])
    -> (DiffT v m ())
    -> (v -> DiffT a m ())
    -> (v -> DiffT a m ())
    -> DiffT a m ()
diffContainer difference intersectWith update add remove = do
    (old, new) <- get
    let removed = difference    old new
        added   = difference    new old
        updated = intersectWith old new
    mapM_ add    added
    mapM_ remove removed
    mapM_ (uncurry (lift .: runDiffT update)) updated
