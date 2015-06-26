{-# LANGUAGE DeriveDataTypeable #-}

import Data.Dynamic
import Data.Maybe   ( fromJust )


data Node = Node { _id       :: Int
                 , _selected :: Bool
                 } deriving (Show, Typeable)

data Foo = Foo deriving (Show, Typeable)


nodeOrig = Node 1 True
nodeDyn  = toDyn nodeOrig
fooDyn   = toDyn $ Foo
arrDyn   = [nodeDyn, fooDyn]

arr  = fmap fromDynamic arrDyn :: [Maybe Node]
node = fromJust . fromDynamic $ nodeDyn :: Node
foo  = fromJust $ fromDynamic fooDyn  :: Foo

main = do
    print arr
    print nodeDyn
    print node
    print fooDyn
    print foo
    -- print (fromDynamic fooDyn :: Maybe Foo)



-- data Dynamic = forall a. (Typeable a) => Dynamic a

-- instance Show Dynamic where
--     show (Dynamic a) = "<<" ++ show (typeOf a) ++ ">>"
