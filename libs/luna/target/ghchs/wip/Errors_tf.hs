{-# LANGUAGE MultiParamTypeClasses #-}

class Catch a b c where
    catch :: a -> b -> c

testme1 x f1 = catch x f1


testme2 :: (Catch a1 b1 a, Catch a b c) => a1 -> b1 -> b -> c
testme2 x f1 f2 = catch (catch x f1) f2

main = print "end"






































































--testme2 :: (Catch a1 b1 a, Catch a b c) => a1 -> b1 -> b -> c
--testme2 x f1 f2 = catch (catch x f1) f2
