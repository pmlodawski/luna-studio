{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}


--{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE ViewPatterns #-}

--{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
----{-# LANGUAGE IncoherentInstances #-}

-- !{-# LANGUAGE RightSideContexts #-}

data Pure a = Pure a deriving Show

myId (Pure a) = (Pure a)

myIdT :: Pure Int -> Pure Int
myIdT a = a

class Pipe a b c | a b -> c where
    pipe :: a -> b -> c

(>>>) = pipe


instance (m1~m2)                                       => Pipe (m1 a  -> b) (m2 a)  b   where pipe f a = f a
instance (a1~a2, m1~m1, Pipe (m1 a1 -> b) (m1 a1) out) => Pipe (m1 a1 -> b) (m2 a2) out where pipe = pipe

main = do
    print $ myIdT >>> Pure (1::Int)
    print $ myId  >>> Pure (1::Int)
    print "end"