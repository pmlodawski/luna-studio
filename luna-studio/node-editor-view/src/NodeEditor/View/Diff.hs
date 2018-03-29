module NodeEditor.View.Diff where

import           Common.Prelude            hiding (get)
import           Control.Monad.Trans.State
import           Data.Convert              (Convertible)
import           Data.Hashable             (Hashable)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap


type DiffT a = StateT (a, a)

runDiffT :: Monad m => DiffT a m r -> a -> a -> m r
runDiffT fun = curry $ evalStateT fun

diff :: Monad m => DiffT b m r -> Getting b a b -> DiffT a m r
diff fun getting = do
    (old, new) <- get
    lift $ runDiffT fun (new ^. getting) (old ^. getting)

diffApply :: (Eq a, Monad m) => (a -> DiffT a m ()) -> DiffT a m ()
diffApply apply = do
    (old, new) <- get
    when (new /= old) $ apply new

diffConvert :: (Convertible a b, Monad m) => DiffT b m r -> DiffT a m r
diffConvert = flip diff $ to convert

diffHashMap :: (Eq k, Eq v, Hashable k, Monad m)
            => (DiffT v m ())
            -> (v -> DiffT (HashMap k v) m ())
            -> (v -> DiffT (HashMap k v) m ())
            -> DiffT (HashMap k v) m ()
diffHashMap update add remove = do
    (old, new) <- get
    let removed = HashMap.elems $ HashMap.difference new old
        added   = HashMap.elems $ HashMap.difference old new
        updated = HashMap.elems $ HashMap.intersectionWith (,) old new
    mapM_ add    added
    mapM_ remove removed
    mapM_ (uncurry (lift .: runDiffT update)) updated
