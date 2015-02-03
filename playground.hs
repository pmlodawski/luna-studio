{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (lookup)
import Control.Lens
import Control.Applicative
import Control.Lens.Iso
import Data.Proxy
import Data.Packable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap


-- newtype MyGenIntMap k v = (Iso' k Int) => MyGenIntMap v

--data MyGenIntMap k v where
--  (Num v) => Proxy k -> v -> MyGenIntMap k v

newtype MyInt = MyInt { fromMyInt :: Int }


instance Show MyInt       where show = show . fromMyInt
instance Pack   Int MyInt where pack = MyInt
instance Unpack MyInt Int where unpack = fromMyInt
instance Packable MyInt Int



--------------------------------------------------------------------------------



data GenIntMap k v where GenIntMap :: (Unpack k Int) => Proxy k -> IntMap v -> GenIntMap k v


lookup :: (Unpack k Int) => k -> GenIntMap k v -> Maybe v
lookup k (GenIntMap _ mp) = IntMap.lookup (unpack k) mp

insert :: (Unpack k Int) => k -> v -> GenIntMap k v -> GenIntMap k v
insert k v (GenIntMap _ mp) = GenIntMap (Proxy::Proxy k) (IntMap.insert (unpack k) v mp)

keys :: (Packable k Int) => GenIntMap k v -> [k]
keys (GenIntMap _ mp) = pack <$> IntMap.keys mp

main :: IO ()
main = do
    let mymap0 = GenIntMap (Proxy::Proxy MyInt) (IntMap.empty :: IntMap String)
        --mymapE = GenIntMap (Proxy::Proxy Char) (IntMap.empty :: IntMap String)
        mymap1 = insert (MyInt 123) "dupa" mymap0
    print (lookup (MyInt 123) mymap1)
    print (keys mymap1)
    putStrLn "ohai"
