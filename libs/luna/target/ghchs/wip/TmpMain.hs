{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

import Tmp

myId2 (Pure a) = (Pure a)

myIdT2 :: Pure Int -> Pure Int
myIdT2 a = a

main = do
    print $ myIdT2 `pipe` Pure (1::Int)
    print $ myId2 `pipe` Pure (1::Int)
    print "end"