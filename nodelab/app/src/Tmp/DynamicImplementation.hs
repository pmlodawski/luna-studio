{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Typeable hiding (cast)
import Unsafe.Coerce (unsafeCoerce)

data Object where
    Object :: Typeable a => a -> Object

instance Show Object where
    show _ = "Object"

data Node = Node { _id       :: Int
                 , _selected :: Bool
                 } deriving (Show, Typeable)

data Foo = Foo deriving (Show, Typeable)

toProxy :: a -> Proxy a
toProxy _ = Proxy

checkObjectType :: Typeable a => Proxy a -> Object -> Bool
checkObjectType (_ :: Proxy a) (Object obj) = typeOf obj == typeOf (undefined :: a)

isNode = checkObjectType (Proxy :: Proxy Node)

main = do 
    print "hello"
    let n = Object $ Node 1 True
        f = Object $ Foo
        objs = [n,f]
    print $ fmap isNode objs
    print $ (cast f :: Maybe Node)
    -- print $ cast (Proxy :: Proxy Node) n
    -- print $ unpack f
    -- print $ unpack n
    -- print $ isNode f

-- unpack :: Object -> a
-- unpack (Object a) = unsafeCoerce a
-- data Proxy a = Proxy

cast :: forall a. Typeable a => Object -> Maybe a
cast obj@(Object el) = if checkObjectType (Proxy :: Proxy a) obj then Just $ unsafeCoerce el
                                                                 else Nothing

-- cast :: 
-- cast p obj = if checkObjectType p obj then 