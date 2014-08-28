{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE OverlappingInstances      #-}

{-# LANGUAGE UndecidableInstances      #-}

import Control.Applicative    hiding (pure)
import Control.Monad.IO.Class
import Control.Monad.Trans
--import Control.Monad.State

--import Bind2 (bind, bind2, MonadRebase(..), StateT(..), put,get)

--import Data2

import Utils



data Err1 = Err1 deriving Show
data Err2 = Err2 deriving Show
data Err3 = Err3 deriving Show
data Err4 = Err4 deriving Show
data Err5 = Err5 deriving Show


newtype Safe a = Safe a deriving Show

fromSafe (Safe a) = a


data UnsafeBase base err val = Value val
                             | Error err
                             | Other (base val)
                             deriving Show

type Unsafe = UnsafeBase Safe

data NOP a = NOP a deriving Show

instance Functor base => Functor (UnsafeBase base err) where
  fmap f a = case a of
      Value a -> Value $ f a
      Error e -> Error e
      Other b -> Other $ fmap f b



class Raise e a b | e a -> b where
    raise :: e -> a -> b

instance Raise e (Safe a) (Unsafe e a) where
    raise e (Safe a) = Error e

instance Raise e (UnsafeBase base be a) (UnsafeBase (UnsafeBase base be) e a) where
    raise e a = Error e




class Catch a b c | a b -> c where
    catch :: a -> b -> c

instance Catch (Safe a) f (Safe a) where
    catch a _ = a


instance Catch (UnsafeBase Safe e a) (e -> Safe a) (Safe a) where
    catch a f = case a of
        Value a -> Safe a
        Error e -> f e


--instance Catch (UnsafeBase NOP e a) (e2 -> Safe a) (UnsafeBase NOP e a) where
--    catch a f = undefined


instance Catch (UnsafeBase (UnsafeBase base e2) e a) (e -> Safe a) (UnsafeBase base e2 a) where
    catch a f = case a of
        Value a -> Value a
        Error e -> Value . fromSafe $ f e
        Other o -> o



--instance (Catch (UnsafeBase base e2 a) (e -> Safe a) (dstBase a)) => Catch (UnsafeBase (UnsafeBase base e2) e3 a) (e -> Safe a) (UnsafeBase dstBase e3 a) where
--    catch base f = case base of
--        Value a     -> Value a
--        Error e     -> Error e
--        Other base' -> Other $ catch base' f



instance (Catch (UnsafeBase base e2 a) (e -> Safe a) (dstBase a)) => Catch (UnsafeBase (UnsafeBase base e2) e3 a) (e -> Safe a) (UnsafeBase dstBase e3 a) where
    catch base f = case base of
        Value a     -> Value a
        Error e     -> Error e
        Other base' -> Other $ catch base' f



test :: Unsafe Err1 Int
test = Value (5::Int)

main = do
    let x = Safe(1::Int)
        ex1 = raise Err1 x
        ex2 = raise Err2 $ raise Err1 x
        ex3 = raise Err3 $ raise Err2 $ raise Err1 x
        ex4 = raise Err4 $ raise Err3 $ raise Err2 $ raise Err1 x
        exa = raise Err4 $ raise Err3 $ raise Err2 x

    --print $ raise Err2 $ raise Err x

    let f1 = (\Err1 -> Safe(0::Int))
    let f2 = (\Err2 -> Safe(0::Int))

    print $ ex4
    --let a = exa
        --a ::  UnsafeBase (UnsafeBase (UnsafeBase Safe Err2) Err3) Err4 Int
        --a :: UnsafeBase (UnsafeBase (UnsafeBase NOP Err2) Err3) Err4 Int
    --print $ catch (catch ex4 (\Err2-> Safe(0::Int))) (\Err3-> Safe(0::Int))
    --print $ catch (catch ex4 (\Err2-> Safe(0::Int))) (\Err3-> Safe(0::Int))

    print "end"
