
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}

{-# LANGUAGE UndecidableInstances #-}


!{-# LANGUAGE RightSideContexts #-}

import Control.Applicative    hiding(pure)
import Control.Monad.IO.Class
import Control.Monad.Trans
--import Control.Monad.State

--import Bind2 (bind, bind2, MonadRebase(..), StateT(..), put,get)

--import Data2

import Utils


foo x y = x

data Err1 = Err1 deriving Show
data Err2 = Err2 deriving Show
data Err3 = Err3 deriving Show
data Err4 = Err4 deriving Show
data Err5 = Err5 deriving Show


newtype Safe a = Safe a deriving Show

fromSafe (Safe a) = a


instance Functor Safe where
    fmap f (Safe a) = Safe (f a)


data UnsafeBase base err val = Value val
                             | Error err
                             | Other (base val)
                             deriving Show

type Unsafe = UnsafeBase Safe

data NOP a = NOP a deriving Show

instance Functor (UnsafeBase base err) <= Functor base where
  fmap f a = case a of
      Value a -> Value $ f a
      Error e -> Error e
      Other b -> Other $ fmap f b


----------------------------------------------------------------------------


class Raise e a b | e a -> b where
    raise :: e -> a -> b 


instance Raise e (Safe a) (UnsafeBase Safe e a) where
    raise e (Safe a) = Error e


instance Raise e (UnsafeBase base e a) (UnsafeBase base e a) where
    raise e a = Error e


instance Raise e (UnsafeBase base be a) (UnsafeBase outBase be a) <= (Raise e (base a) (outBase a)) where
    raise e base = case base of
        Value val   -> Value val
        Error err   -> Error err
        Other base' -> Other $ raise e base'


----------------------------------------------------------------------------


class Catch value handler result | value handler -> result where
    catch :: handler -> value -> result


instance Catch (Safe a) f (Safe a) where
    catch _ a = a


instance Catch (UnsafeBase Safe e a)                  (e -> Safe a) (Safe a)                  where
    catch f a = case a of
        Value a -> Safe a
        Error e -> f e
instance Catch (UnsafeBase (UnsafeBase base e2) e a)  (e -> Safe a) (UnsafeBase base e2 a)    where
    catch f a = case a of
        Value a -> Value a
        Error e -> Value . fromSafe $ f e
        Other o -> o
instance Catch (UnsafeBase (UnsafeBase base e2) e3 a) (e -> Safe a) (UnsafeBase dstBase e3 a) <= (Catch (UnsafeBase base e2 a) (e -> Safe a) (dstBase a)) where
    catch f base = case base of
        Value a     -> Value a
        Error e     -> Error e
        Other base' -> Other $ catch f base'



test :: Unsafe Err1 Int
test = Value (5::Int)

main = do
    let x = Safe(1::Int)
        ex1 = raise Err1 x
        ex2 = raise Err1 $ raise Err2 x
        ex3 = raise Err3 $ raise Err2 $ raise Err1 x
        ex4 = raise Err4 $ raise Err3 $ raise Err2 $ raise Err1 x
        exa = raise Err4 $ raise Err3 $ raise Err2 x


    print ex4
    print $ fmap (+10) ex4
    
    print $ catch (\Err1 -> Safe(0::Int)) $ ex2
    print $ catch (\Err1 -> Safe(0::Int)) 
          . catch (\Err2 -> Safe(0::Int)) 
          $ ex2

    print "end"
