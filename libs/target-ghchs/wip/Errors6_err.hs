{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

data UnsafeBase base err val = Value val
                             | Error err
                             | Other (base val)
                             deriving (Show, Eq)

class MagicMerge m1 m2 | m1 -> m2 where
    magicMerge :: m1 a -> m2 a


instance MagicMerge (UnsafeBase (UnsafeBase base err) err) (UnsafeBase base err) where
    magicMerge ma = case ma of
        Value a -> Value a
        Error e -> Error e
        Other o -> o

instance (MagicMerge (UnsafeBase base err2) dstBase) => MagicMerge (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase dstBase err1) where
    magicMerge (ma :: UnsafeBase (UnsafeBase base err1) err2 a) = case ma of
        Value a -> Value a
        Error e -> Other $ magicMerge (Error e :: UnsafeBase base err2 a)
        Other o -> case o of
            Value a  -> Other $ magicMerge (Value a  :: UnsafeBase base err2 a)
            Error e  -> Error e
            Other o' -> Other $ magicMerge (Other o' :: UnsafeBase base err2 a)


main = print "help me world!"